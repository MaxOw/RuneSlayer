module Entity.Player
    ( Player, playerToEntity
    , makePlayer
    ) where

import Delude
import Data.Generics.Product.Subtype (upcast)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Diagrams.TwoD.Transform as T

import Entity
import Types.Debug
import Types.Entity.Reactivity
import Types.Entity.Player
import Types.Entity.Effect
import Types.Entity.Item (WeaponKind(..))
import Entity.Utils
import Entity.Actions
import Types.Entity.Timer
import ResourceManager (Resources, lookupAnimation)
import Types.Equipment
import qualified Equipment
import Skills.Runes

import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color

import qualified Entity.Animation as Animation

--------------------------------------------------------------------------------

actOn :: Player -> EntityAction -> Player
actOn x a = x & case a of
    EntityAction_ToggleDebug       f -> toggleDebugFlag f
    EntityAction_DebugRunAnimation k -> setAnimationKind k
    EntityAction_SetMoveVector     v -> setMoveVector v
    EntityAction_DropAllItems        -> handleOnUpdate a
    EntityAction_AddItem           _ -> handleOnUpdate a
    EntityAction_DropItem          _ -> handleOnUpdate a
    EntityAction_OwnerDropItem     _ -> handleOnUpdate a
    EntityAction_ExecuteAttack       -> handleOnUpdate a
    EntityAction_SelfAttacked      _ -> handleOnUpdate a
    EntityAction_PlayerAction      p -> handlePlayerAction p
    _ -> id
    where
    setAnimationKind k _ = x
        & animationState.current.kind .~ k
        & animationState.current.era  .~ 0
        & animationState.progression  .~ Animation.defaultTransition

    handlePlayerAction = \case
        PlayerAction_SelectRune        -> selectCurrentRune
        PlayerAction_UpdateRune   rt r -> updateCurrentRune rt r
        PlayerAction_SetAttackMode  am -> setAttackMode am

    currentTime = undefined
    selectCurrentRune = case x^.ff#selectedRune of
        Nothing -> ff#selectedRune .~ selectRune currentTime (x^.ff#runicLevel)
        Just  _ -> id

    updateCurrentRune rt r _ = x
        & ff#selectedRune .~ Nothing
        & loadSlot rt r
        & updateRuneUsage r
    loadSlot RuneType_Offensive True = ff#offensiveSlots %~ fillRunicSlot
    loadSlot RuneType_Defensive True = ff#defensiveSlots %~ fillRunicSlot
    loadSlot _ _ = id

    updateRuneUsage r = case x^.ff#selectedRune of
        Nothing -> id
        Just sn -> over (ff#runicLevel) $ updateUsage sn (RuneUsage r)

    setAttackMode     = set (ff#attackMode)

update :: Player -> EntityContext -> Q (Maybe Player, [DirectedAction])
update x ctx = runUpdate x ctx $ do
    runAttackMode
    updateAnimationState
    updateTimer
    updateDelayedActions
    playerIntegrateLocation
    separateCollision
    autoTarget
    whenMatch _EntityAction_AddItem      addItems
    whenMatch _EntityAction_DropAllItems dropAllItems
    anyMatch  _EntityAction_SelfAttacked procAttacked
    mapM_ processUpdateOnce =<< use (self.ff#updateOnce)
    mapM_ processAction =<< use (self.processOnUpdate)
    runicActions
    self.processOnUpdate .= mempty

runicActions :: Update Player ()
runicActions = do
    -- Detect hostiles
    ds <- queryInRadius EntityKind_Dynamic hostileDetectionRange
    if any isHostile ds
    then runicMode
    else endRunicMode
    where
    hostileDetectionRange = disM 8
    isHostile e
        = not (Set.null rset || rset == Set.singleton ReactivCategory_Life)
        where
        rset = Map.keysSet $ fromMaybe mempty $ e^.entity.oracleReactivity

    runicMode = do
        self.status %= Set.insert EntityStatus_HostilesInRange
        return ()

    endRunicMode = do
        self.status %= Set.delete EntityStatus_HostilesInRange
        return ()

processUpdateOnce :: UpdateOnce -> Update Player ()
processUpdateOnce = \case
    UpdateOnce_Equipment -> updateEquipment
    where
    updateEquipment = do
        eis <- uses (self.equipment) Equipment.slotsList
        let seis = map snd $ sortVia fst equipmentRenderOrder eis
        es <- catMaybes <$> mapM queryById seis
        let as = mapMaybe (view (entity.oracleItemAnimation)) es
        rs <- use $ context.resources
        let eqAnim = mconcat $ mapMaybe (flip lookupAnimation rs) as
        self.ff#equipmentAnimation .= eqAnim

updateDelayedActions :: Update Player ()
updateDelayedActions = do
    as <- use $ self.ff#delayedActions
    us <- catMaybes <$> mapM upd as
    self.ff#delayedActions .= us
    where
    upd a = let nt = a^.ff#timeLeft - defaultDelta in if
      | nt <= 0   -> performDelayedAction (a^.ff#action) >> return Nothing
      | otherwise -> return $ Just $ a & ff#timeLeft .~ nt

performDelayedAction :: DelayedActionType -> Update Player ()
performDelayedAction = \case
   DelayedActionType_Attack eid p -> addAction eid $ EntityAction_SelfAttacked p

addDelayedAction :: Duration -> DelayedActionType -> Update Player ()
addDelayedAction d t = self.ff#delayedActions %= (DelayedAction d t:)

procAttacked :: NonEmpty AttackPower -> Update Player ()
procAttacked = mapM_ (addEffect . HitEffect) <=< mapM applyDefence
    where
    applyDefence x = do
        ad <- uses (self.ff#defensiveSlots) (any (>0) . listRunicSlots)
        let runicDefence = 1
        self.ff#defensiveSlots %= dischargeRunicSlot
        return $ max 0 $ if ad then x-runicDefence else x

runAttackMode :: Update Player ()
runAttackMode = use (self.ff#attackMode) >>= \case
    AttackMode_Manual -> whenMatch _EntityAction_ExecuteAttack executeAttack
    AttackMode_Auto   -> executeAttack

executeAttack :: Update Player ()
executeAttack = whenM canExecuteAttack $ do
    mt <- fmap join . mapM queryById =<< use (self.target)
    whenJust mt $ \te -> do
        let mloc = te^.entity.oracleLocation
        whenJust mloc $ \loc -> do
            sloc <- use $ self.location
            executeAttackAt te $ loc^._Wrapped - sloc^._Wrapped

canExecuteAttack :: Update Player Bool
canExecuteAttack = (&&)
    <$> checkTimeUp Timer_Attack
    <*> anyOffensiveRuneLoaded
    where
    anyOffensiveRuneLoaded
        = uses (self.ff#offensiveSlots) (any (>0) . listRunicSlots)

getWeaponKind :: Update Player WeaponKind
getWeaponKind = do
    mei <- getEquippedItem EquipmentSlot_Weapon
    let mwk = mei^?traverse.entity.oracleItemType.traverse.weaponKind.traverse
    return $ fromMaybe WeaponKind_Slashing mwk

executeAttackAt :: EntityWithId -> V2 Float -> Update Player ()
executeAttackAt targetEntity vectorToTarget = do
    wkind <- getWeaponKind
    whenInAttackRange wkind targetEntity $ do
        startAttackAnimation wkind vectorToTarget
        unless (wkind == WeaponKind_Projecting) $ do
            attackPower <- getAttackPower
            attackDelay <- getAttackDelay
            addDelayedAction attackDelay $
                DelayedActionType_Attack (targetEntity^.entityId) attackPower
        startTimer Timer_Attack =<< getAttackCooldown
        self.ff#offensiveSlots %= dischargeRunicSlot

startAttackAnimation :: WeaponKind -> V2 Float -> Update Player ()
startAttackAnimation wkind vectorToTarget = do
    -- TODO: Choose attack animation based on weapon
    self.animationState.current.direction %= Animation.vecToDir vectorToTarget
    self.animationState.current.kind .= animkind
    self.animationState.current.era  .= 0
    self.animationState.progression  .= Animation.defaultTransition
    where
    animkind = case wkind of
        WeaponKind_Slashing   -> Animation.Slash
        WeaponKind_Thrusting  -> Animation.Thrust
        WeaponKind_Projecting -> Animation.Fire

whenInAttackRange
    :: HasLocation s Location
    => HasEntity e Entity
    => WeaponKind
    -> e -> Update s () -> Update s ()
whenInAttackRange wkind targetEntity act = do
    attackRange <- getAttackRange wkind
    dist <- distanceToEntity attackRange targetEntity
    when (dist < attackRange) act

getAttackRange :: WeaponKind -> Update x Distance
getAttackRange = \case
    WeaponKind_Slashing   -> return $ Distance 2
    WeaponKind_Thrusting  -> return $ Distance 3
    WeaponKind_Projecting -> return $ Distance 8

getAttackPower :: Update x AttackPower
getAttackPower = return $ AttackPower 1
    -- TODO: Calculate attack power

getAttackDelay :: Update x Duration
getAttackDelay = return $ timeInSeconds 0.7

getAttackCooldown :: Update x Duration
getAttackCooldown = return $ timeInSeconds 1.2

playerIntegrateLocation :: Update Player ()
playerIntegrateLocation = do
    k <- use $ self.animationState.current.kind
    when (k == Animation.Walk) integrateLocation

autoTarget :: Update Player ()
autoTarget = do
    ds <- queryInRadius EntityKind_Dynamic (disM 8)
    sid <- use $ context.selfId
    loc <- use $ self.location
    -- TODO: fix auto targeting reactivity list
    let ht = Set.fromList [ReactivCategory_Shadow]
    let hs = sortWith (distanceTo loc) $ filter (shouldTarget sid ht) ds
    let newTarget = viaNonEmpty head (map (view entityId) hs)
    currentTarget <- use $ self.target
    self.target .= viaNonEmpty head (map (view entityId) hs)
    when (newTarget /= currentTarget) $ do
        mapM_ (flip addAction EntityAction_SelfUnmarkAsTarget) currentTarget
        mapM_ (flip addAction EntityAction_SelfMarkAsTarget) newTarget

    where
    shouldTarget sid ht x = x^.entityId /= sid && isHostileTo ht x
    distanceTo loc e = fromMaybe 10000 $
        (distance (loc^._Wrapped) . view _Wrapped <$> e^.entity.oracleLocation)

processAction :: EntityAction -> Update Player ()
processAction = \case
    EntityAction_DropItem i -> dropItem i
    EntityAction_OwnerDropItem i -> dropItemAction i
    _ -> return ()

--------------------------------------------------------------------------------

render :: Player -> RenderContext -> RenderAction
render x ctx = withZIndex x $ locate x $ renderComposition
    [ renderDebug
    , translateY 0.8 $ renderComposition
        [ renderBody
        , renderEquipment
        ]
    ]
    where
    renderDebug     = renderComposition $ localDebug <> globalDebug

    renderAnim      = Animation.renderAnimation (x^.animationState)
    renderBody      = renderAnim (x^.ff#bodyAnimation)
    renderEquipment = renderAnim (x^.ff#equipmentAnimation)

    localDebug = map snd
        $ filter (\(f, _) -> x^.debugFlags.f)
        [ (drawPickupRange, renderPickupRange)
        ]

    globalDebug = map snd
        $ filter (\(f, _) -> Set.member f $ ctx^.debugFlags)
        [ (DebugFlag_ShowCollisionShapes, renderCollisionShape cs)
        ]

    cs = x^.collisionShape

    rangeScale = defaultPickupRange^._Wrapped
    renderPickupRange = T.scale rangeScale $ renderShape $ def
        & shapeType   .~ SimpleCircle
        & color       .~ Color.withOpacity Color.red 0.3

oracle :: Player -> EntityQuery a -> Maybe a
oracle x = \case
    EntityQuery_Location       -> Just $ x^.location
    EntityQuery_Equipment      -> Just $ x^.equipment
    EntityQuery_CollisionShape -> locate x <$> x^.collisionShape
    EntityQuery_Reactivity     -> Just $ x^.reactivity
    EntityQuery_Status         -> Just $ x^.status
    EntityQuery_PlayerStatus   -> Just $ upcast x
    _                          -> Nothing

--------------------------------------------------------------------------------

playerToEntity :: Player -> Entity
playerToEntity = makeEntity $ EntityParts
   { makeActOn  = actOn
   , makeUpdate = update
   , makeRender = render
   , makeOracle = oracle
   , makeSave   = Just . EntitySum_Player
   , makeKind   = EntityKind_Dynamic
   }

makePlayer :: Resources -> PlayerInit -> Player
makePlayer rs p = def
    & ff#bodyAnimation  .~ as
    & reactivity        .~ (p^.reactivity)
    & maxSpeed          .~ (p^.maxSpeed)
    & equipment         .~ Equipment.create playerSlots
    & ff#attackRange    .~ disM 2
    & ff#offensiveSlots .~ initRunicSlots 4
    & ff#defensiveSlots .~ initRunicSlots 3
    & ff#runicLevel     .~ initKnownRunes
    where
    as = mconcat $ mapMaybe (flip lookupAnimation rs) (p^.body)
    initRunes = getRunesByLevel 1 (rs^.runeSet)
    initKnownRunes = addKnownRunes initRunes def

