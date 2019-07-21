module Entity.Agent
    ( Agent, agentToEntity
    , makeAgent
    ) where

import Delude
import Data.Generics.Product.Subtype (upcast)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Diagrams.TwoD.Transform as T

import Entity
import Entity.HasField
import Types.Debug
import Types.Entity.Reactivity
import Types.Entity.Agent
import Types.Entity.Effect
import Types.Entity.Passive
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

actOn :: Agent -> EntityAction -> Agent
actOn x a = x & case a of
    EntityAction_SelfMarkAsTarget    -> set isMarked True
    EntityAction_SelfUnmarkAsTarget  -> set isMarked False
    -- Handle here:
    EntityAction_ToggleDebug       f -> toggleDebugFlag f
    EntityAction_RunAnimation      k -> setAnimationKind k
    EntityAction_SetMoveVector     v -> setMoveVector v
    EntityAction_SelfHeal          h -> selfHeal h
    EntityAction_PlayerAction      p -> handlePlayerAction p
    EntityAction_SetValue          v -> handleSetValue v
    -- Handle on update:
    EntityAction_AddItem          {} -> handleOnUpdate a
    EntityAction_RemoveItem       {} -> handleOnUpdate a

    EntityAction_ExecuteAttack       -> handleOnUpdate a
    EntityAction_SelfAttacked      _ -> handleOnUpdate a
    EntityAction_UseItem           _ -> handleOnUpdate a
    _ -> id
    where
    selfHeal h _ = x & health %~
        (\ch -> min (x^.fullStats.maxHealth) (ch+h))

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

    handleSetValue ev _ = case ev of
        EntityValue_Location     v -> x & location .~ v
        EntityValue_Direction    _ -> x
        EntityValue_Animation    _ -> x

update :: Agent -> EntityContext -> Q (Maybe Agent, [DirectedAction])
update x ctx = runUpdate x ctx $ do
    decideAction

    updateActiveAnimation
    updateTimer
    updateDelayedActions
    integrateLocationWhenWalking
    separateCollision

    allMatch _EntityAction_AddItem (addItems . toList)
    mapM_ processAction =<< use (self.processOnUpdate)
    mapM_ processUpdateOnce =<< use (self.updateOnce)
    self.processOnUpdate .= mempty

decideAction :: Update Agent ()
decideAction = useAgentKind >>= \case
    AgentKind_Player -> playerActions
    AgentKind_Enemy  -> whenJustM useUnitType enemyActions
    AgentKind_NPC    -> npcActions
    where
    useUnitType = use $ self.agentType.ff#unitType
    playerActions = do
        runAttackMode
        autoTarget
        runicActions

    enemyActions enemy = do
        self.velocity .= 0
        selectTarget enemy
        pursueOrAttackTarget enemy
        spreadOut enemy

    npcActions = do
        return ()

    selectTarget enemy = whenNothingM_ (use $ self.target) $ do
        let aggroRange = enemy^.ff#aggroRange
        let hostiles = isHostileTo $ enemy^.ff#hostileTowards
        es <- queryInRange EntityKind_Dynamic aggroRange
        let targetId = view entityId <$> listToMaybe (filter hostiles es)
        self.target .= targetId

    getTarget = bindMaybeM (use $ self.target) queryById
    pursueOrAttackTarget enemy = whenJustM getTarget $ \targetEntity -> do
        let attackRange = enemy^.ff#attackRange
        let pursueRange = enemy^.ff#pursueRange
        dist <- fromMaybe pursueRange <$> distanceToEntity targetEntity
        orientTowards targetEntity
        if dist < attackRange
        then attackTarget enemy targetEntity
        else if dist < pursueRange
            then moveTowards targetEntity
            else self.target .= Nothing

    -- attackTarget :: EntityWithId -> Update Agent ()
    attackTarget enemy targetEntity = whenM (checkTimeUp Timer_Attack) $ do
        attackPower <- getAttackPower
        let attackSpeed = enemy^.ff#attackSpeed
        addAction targetEntity $ EntityAction_SelfAttacked attackPower
        selectAnimation Animation.Slash
        startTimer Timer_Attack attackSpeed

    spreadOut enemy = do
        let disperseRange = distanceInMeters 0.5
        es <- queryInRadius EntityKind_Dynamic disperseRange
        let hostiles = isHostileTo $ enemy^.ff#hostileTowards
        let ts = filter (not . hostiles) es
        vs <- catMaybes <$> mapM vectorToEntity ts
        let v = sum $ map (negate . normalize) vs
        s <- use $ self.maxSpeed
        self.velocity += velocityFromSpeed v (s*0.3)

runicActions :: Update Agent ()
runicActions = do
    -- Detect hostiles
    ds <- queryInRange EntityKind_Dynamic hostileDetectionRange
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

processUpdateOnce :: UpdateOnce -> Update Agent ()
processUpdateOnce = \case
    UpdateOnce_Equipment -> updateEquipment
    where
    updateEquipment = do
        (eqBehindAnim, eqAnim) <- makeEquipmentAnimationVia equipmentRenderOrder
        bodyAnim <- makeBodyAnimation
        projAnim <- makeProjectileAnimation
        self.animation .= eqBehindAnim <> bodyAnim <> eqAnim <> projAnim
        updateStats

    makeEquipmentAnimationVia renderOrder = do
        eis <- uses (self.equipment) Equipment.slotsList
        let seis = map snd $ sortSelectVia fst renderOrder eis
        es <- catMaybes <$> mapM queryById seis
        let (bs, fs) = List.partition isBehind es
        rs <- use $ context.resources
        return (makeAnim rs bs, makeAnim rs fs)

    makeProjectileAnimation = selectProjectile >>= \case
        Nothing -> return mempty
        Just pr -> do
            mpe <- queryById pr
            rs <- use $ context.resources
            return $ fromMaybe mempty $ makeAnim rs . (:[]) <$> mpe

    makeAnim rs
        = mconcat . mapMaybe (flip lookupAnimation rs)
        . mapMaybe (view (entity.oracleItemAnimation))

    isBehind x = fromMaybe False $ x^.entity.oracleBehindBody

    makeBodyAnimation = do
        bns <- use (self.agentType.bodyAnimation)
        rs <- use $ context.resources
        return $ mconcat $ mapMaybe (flip lookupAnimation rs) bns

updateStats :: Update Agent ()
updateStats = do
    eis <- uses (self.equipment) Equipment.contentList
    es  <- catMaybes <$> mapM queryById eis
    let ss = mapMaybe (view (entity.oracleStats)) es
    bs <- use $ self.baseStats
    self.fullStats .= Stats
        { field_attack    = bs^.ff#attack    + sumOf (traverse.ff#attack)    ss
        , field_defence   = bs^.ff#defence   + sumOf (traverse.ff#defence)   ss
        , field_maxHealth = bs^.ff#maxHealth + sumOf (traverse.ff#maxHealth) ss
        , field_maxSpeed  = bs^.ff#maxSpeed  + sumOf (traverse.ff#maxSpeed)  ss
        }

updateDelayedActions :: Update Agent ()
updateDelayedActions = do
    as <- use $ self.ff#delayedActions
    us <- catMaybes <$> mapM upd as
    self.ff#delayedActions .= us
    where
    upd a = let nt = a^.ff#timeLeft - defaultDelta in if
      | nt <= 0   -> performDelayedAction (a^.ff#action) >> return Nothing
      | otherwise -> return $ Just $ a & ff#timeLeft .~ nt

performDelayedAction :: DelayedActionType -> Update Agent ()
performDelayedAction = \case
    DelayedActionType_Attack           eid p -> attack eid p
    DelayedActionType_FireProjectile v tid p -> fireProjectile v tid p
    where
    attack eid p = addAction eid $ EntityAction_SelfAttacked p
    fireProjectile v tid p = whenJustM selectProjectile $ \pid -> do
        loc <- use $ self.location
        addAction pid $ EntityAction_SelfFiredAsProjectile loc v tid p

selectProjectile :: Update Agent (Maybe EntityId)
selectProjectile = getFirstArrow <$> getEquippedItem EquipmentSlot_PrimaryOther
    where
    getFirstArrow x = viaNonEmpty head $ x^.traverse.entity.oracleContent.traverse

addDelayedAction :: Duration -> DelayedActionType -> Update Agent ()
addDelayedAction d t = self.ff#delayedActions %= (DelayedAction d t:)

runAttackMode :: Update Agent ()
runAttackMode = use (self.ff#attackMode) >>= \case
    AttackMode_Manual -> whenMatch _EntityAction_ExecuteAttack executeAttack
    AttackMode_Auto   -> executeAttack

executeAttack :: Update Agent ()
executeAttack = whenM canExecuteAttack $ do
    mt <- fmap join . mapM queryById =<< use (self.target)
    whenJust mt $ \te -> do
        let mloc = te^.entity.oracleLocation
        whenJust mloc $ \loc -> do
            sloc <- use $ self.location
            executeAttackAt te $ loc^._Wrapped - sloc^._Wrapped

canExecuteAttack :: Update Agent Bool
canExecuteAttack = (&&)
    <$> checkTimeUp Timer_Attack
    <*> anyOffensiveRuneLoaded
    where
    anyOffensiveRuneLoaded
        = uses (self.ff#offensiveSlots) (any (>0) . listRunicSlots)

getWeaponKind :: Update Agent WeaponKind
getWeaponKind = do
    mei <- getEquippedItem EquipmentSlot_PrimaryWeapon
    let mwk = mei^?traverse.entity.oraclePassiveType.traverse.weaponKind.traverse
    return $ fromMaybe WeaponKind_Slashing mwk

executeAttackAt :: EntityWithId -> V2 Float -> Update Agent ()
executeAttackAt targetEntity vectorToTarget = do
    wkind <- getWeaponKind
    whenWeaponCanAttack wkind targetEntity $ do
        startAttackAnimation wkind vectorToTarget
        executeAttackByKind wkind
        startTimer Timer_Attack =<< getAttackCooldown
        self.ff#offensiveSlots %= dischargeRunicSlot
    where
    executeAttackByKind = \case
        WeaponKind_Slashing   -> executeMeleAttack
        WeaponKind_Thrusting  -> executeMeleAttack
        WeaponKind_Projecting -> executeProjectileAttack

    executeMeleAttack = do
        attackPower <- getAttackPower
        attackDelay <- getAttackDelay
        addDelayedAction attackDelay $
            DelayedActionType_Attack (targetEntity^.entityId) attackPower

    executeProjectileAttack = do
        attackPower <- getAttackPower
        attackDelay <- getAttackDelay
        addDelayedAction attackDelay $
            DelayedActionType_FireProjectile
                vectorToTarget
                (targetEntity^.entityId) -- Target Id
                attackPower

startAttackAnimation :: WeaponKind -> V2 Float -> Update Agent ()
startAttackAnimation wkind vectorToTarget = do
    self.animationState.current.direction %= Animation.vecToDir vectorToTarget
    selectAnimation $ case wkind of
        WeaponKind_Slashing   -> Animation.Slash
        WeaponKind_Thrusting  -> Animation.Thrust
        WeaponKind_Projecting -> Animation.Fire

whenWeaponCanAttack
    :: HasEntity e Entity
    => WeaponKind -> e -> Update Agent () -> Update Agent ()
whenWeaponCanAttack wkind targetEntity act = do
    attackRange <- getAttackRange wkind
    dist <- fromMaybe attackRange <$> distanceToEntity targetEntity
    canFire <- canFireProjectile wkind
    when (dist < attackRange && canFire) act
    where
    canFireProjectile WeaponKind_Projecting = isJust <$> selectProjectile
    canFireProjectile _ = return True

getAttackRange :: WeaponKind -> Update x Distance
getAttackRange = \case
    WeaponKind_Slashing   -> return $ Distance 2
    WeaponKind_Thrusting  -> return $ Distance 3
    WeaponKind_Projecting -> return $ Distance 8

getAttackPower :: Update Agent AttackPower
getAttackPower = do
    updateStats
    use (self.fullStats.ff#attack)

getDefence :: Update Agent Defence
getDefence = do
    updateStats
    use (self.fullStats.ff#defence)

getAttackDelay :: Update x Duration
getAttackDelay = return $ timeInSeconds 0.7

getAttackCooldown :: Update x Duration
getAttackCooldown = return $ timeInSeconds 1.2

integrateLocationWhenWalking :: Update Agent ()
integrateLocationWhenWalking = do
    k <- use $ self.animationState.current.kind
    when (k == Animation.Walk) integrateLocation

autoTarget :: Update Agent ()
autoTarget = do
    ds <- queryInRange EntityKind_Dynamic (disM 8)
    sid <- useSelfId
    loc <- use $ self.location
    -- TODO: fix auto targeting reactivity list
    let ht = Set.fromList [ReactivCategory_Shadow]
    let hs = sortWith (distanceTo loc) $ filter (shouldTarget sid ht) ds
    let newTarget = viaNonEmpty head (map (view entityId) hs)
    currentTarget <- use $ self.target
    when (newTarget /= currentTarget) $
      whenM (isCloserBy 0.1 newTarget currentTarget) $ do
        self.target .= newTarget
        mapM_ (flip addAction EntityAction_SelfUnmarkAsTarget) currentTarget
        mapM_ (flip addAction EntityAction_SelfMarkAsTarget) newTarget

    where
    shouldTarget sid ht x = x^.entityId /= sid && isHostileTo ht x
    distanceTo loc e = fromMaybe 10000 $
        (distance (loc^._Wrapped) . view _Wrapped <$> e^.entity.oracleLocation)

    isCloserBy d newTarget currentTarget = do
        newDist <- maybeDistance newTarget
        oldDist <- maybeDistance currentTarget
        return $ fromMaybe2 True newDist oldDist (\a b -> a - b < (-d))

    maybeDistance :: Maybe EntityId -> Update Agent (Maybe Distance)
    maybeDistance Nothing = return Nothing
    maybeDistance (Just eid) = bindMaybeM (queryById eid) distanceToEntity

processAction :: EntityAction -> Update Agent ()
processAction = \case
    EntityAction_UseItem       i -> useItem i
    EntityAction_SelfAttacked  d -> procAttacked d
    EntityAction_RemoveItem    i -> removeItem i
    _ -> return ()

removeItem :: EntityId -> Update Agent ()
removeItem i = do
    self.equipment %= Equipment.deleteId i
    flagUpdate UpdateOnce_Equipment

procAttacked :: AttackPower -> Update Agent ()
procAttacked attackPower = do
    -- ad <- uses (self.ff#defensiveSlots) (any (>0) . listRunicSlots)
    -- self.ff#defensiveSlots %= dischargeRunicSlot
    applyAttackDamage
    whenM shouldDie doDie
    where
    applyAttackDamage = do
        fdef <- getDefence
        let calcDef (AttackPower a) (Defence d) = Health $ a - d
        let ap = max 0 $ calcDef attackPower fdef
        self.health %= max 0 . subtract ap
        addEffect $ HitEffect ap

    doDie = do
        isPlayer <- (AgentKind_Player ==) <$> useAgentKind

        deleteSelf .= True
        whenJustM (use $ self.agentType.corpse) $ \c -> do
            loc <- use $ self.location
            ani <- use $ self.animation
            dir <- use $ self.animationState.current.direction
            addWorldAction $ WorldAction_SpawnEntity (SpawnEntity_Passive c) $ def
                & tagAsCamera .~ isPlayer
                & actions .~
                [ EntityAction_SetValue $ EntityValue_Location  loc
                , EntityAction_SetValue $ EntityValue_Direction dir
                , EntityAction_SetValue $ EntityValue_Animation ani
                , EntityAction_RunAnimation Animation.Die ]

        when isPlayer $ addWorldAction WorldAction_GameOver

useAgentKind :: Update Agent AgentKind
useAgentKind = use $ self.agentType.agentKind

--------------------------------------------------------------------------------

render :: Agent -> RenderContext -> RenderAction
render x ctx = withZIndex x $ locate x $ renderComposition
    [ renderIf (x^.isMarked) renderTargetMark
    , renderDebug
    , addRenderOffset renderAnim
    ]
    where
    renderDebug = renderComposition $ localDebug <> globalDebug
    renderAnim  = Animation.renderAnimation (x^.animationState) (x^.animation)

    addRenderOffset = fromMaybe id $ fmap translate $ x^.agentType.ff#renderOffset

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

oracle :: Agent -> EntityQuery a -> Maybe a
oracle x = \case
    EntityQuery_Name           -> Just $ unAgentTypeName $ x^.agentType.name
    EntityQuery_Location       -> Just $ x^.location
    EntityQuery_Equipment      -> Just $ x^.equipment
    EntityQuery_CollisionShape -> locate x <$> x^.collisionShape
    EntityQuery_Reactivity     -> Just $ x^.agentType.reactivity
    EntityQuery_Status         -> Just $ x^.status
    EntityQuery_PlayerStatus   -> Just $ upcast x
    _                          -> Nothing

--------------------------------------------------------------------------------

agentToEntity :: Agent -> Entity
agentToEntity = makeEntity $ EntityParts
   { makeActOn  = actOn
   , makeUpdate = update
   , makeRender = render
   , makeOracle = oracle
   , makeSave   = Just . EntitySum_Agent
   , makeKind   = const EntityKind_Dynamic
   }

makeAgent :: Resources -> AgentType -> Agent
makeAgent rs p = def
    & equipment         .~ Equipment.create (p^.ff#equipmentSlots)
    & updateOnce        .~ Set.fromList [ UpdateOnce_Equipment ]
    & health            .~ p^.stats.maxHealth
    & baseStats         .~ p^.stats
    & agentType         .~ p

    & ff#offensiveSlots .~ initRunicSlots 4
    & ff#defensiveSlots .~ initRunicSlots 3
    & ff#runicLevel     .~ initKnownRunes
    where
    initRunes = getRunesByLevel 1 (rs^.runeSet)
    initKnownRunes = addKnownRunes initRunes def

