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

import Types.Debug
import Types.Entity.Reactivity
import Types.Entity.Agent
import Types.Entity.Effect
import Types.Entity.Passive
import Types.Entity.Timer
import Types.Equipment

import Entity
import Entity.HasField
import Entity.Utils
import Entity.Actions
import Entity.Script

import Skills.Runes
import ResourceManager (Resources, lookupAnimation)
import qualified Equipment

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

    EntityAction_AddLoadout        _ -> handleOnUpdate a
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
    setupScript
    decideAction
    stepScript

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
        autoTargetMark
        runicActions

    enemyActions eunit = do
        self.velocity .= 0
        autoTarget
        pursueOrAttackTarget eunit
        spreadOut

    npcActions = do
        self.velocity .= 0
        autoTarget
        whenJustM getTarget $ \targetEntity -> do
            attackRange <- getAttackRange
            dist <- fromMaybe (1/0) <$> distanceToEntity targetEntity
            when (dist < attackRange) $ do
                orientTowards targetEntity
                executeAttack

    -- Get target or cleanup when current target is dead.
    getTarget = use (self.target) >>= \case
        Nothing -> return Nothing
        Just ti -> do
            eid <- queryById ti
            self.target .= fmap (view entityId) eid
            return eid

    pursueOrAttackTarget eunit = whenJustM getTarget $ \targetEntity -> do
        attackRange <- getAttackRange
        let pursueRange = eunit^.ff#pursueRange
        dist <- fromMaybe pursueRange <$> distanceToEntity targetEntity
        if dist < attackRange
        then executeAttack -- attackTarget eunit targetEntity
        else if dist < pursueRange
            then orientTowards targetEntity >> moveTowards targetEntity
            else self.target .= Nothing

    spreadOut = do
        let disperseRange = distanceInMeters 0.5
        es <- queryInRadius EntityKind_Dynamic disperseRange
        hostiles <- isHostileTo <$> use (self.agentType.ff#hostileTowards)
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
    wr <- getWeaponAttackRange
    ar <- calcAttackRange wr (sumOf (traverse.ff#attackRange) ss)
    self.fullStats .= Stats
        { field_attack      = bs^.ff#attack    + sumOf (traverse.ff#attack)    ss
        , field_defence     = bs^.ff#defence   + sumOf (traverse.ff#defence)   ss
        , field_maxHealth   = bs^.ff#maxHealth + sumOf (traverse.ff#maxHealth) ss
        , field_maxSpeed    = bs^.ff#maxSpeed  + sumOf (traverse.ff#maxSpeed)  ss
        , field_attackRange = ar
        }
    where
    calcAttackRange wr ssr = getWeaponKind >>= \case
        Nothing                    -> return $ wr + ssr
        Just WeaponKind_Projecting -> return $ wr + ssr
        Just _                     -> return $ wr

    getWeaponAttackRange :: Update Agent Distance
    getWeaponAttackRange = do
        mei <- getEquippedItem EquipmentSlot_PrimaryWeapon
        br <- use $ self.baseStats.ff#attackRange
        let mwr = mei^?traverse.entity.oracleStats.traverse.ff#attackRange
        return $ fromMaybe br mwr

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
    anyOffensiveRuneLoaded = useAgentKind >>= \x -> if x == AgentKind_Player
        then uses (self.ff#offensiveSlots) (any (>0) . listRunicSlots)
        else return True

getWeaponKind :: Update Agent (Maybe WeaponKind)
getWeaponKind = do
    mei <- getEquippedItem EquipmentSlot_PrimaryWeapon
    let mwk = mei^?traverse.entity.oraclePassiveType.traverse.weaponKind.traverse
    return mwk

executeAttackAt :: EntityWithId -> V2 Float -> Update Agent ()
executeAttackAt targetEntity vectorToTarget = do
    wkind <- fromMaybe WeaponKind_Slashing <$> getWeaponKind
    whenWeaponCanAttack wkind targetEntity $ do
        orientTowards targetEntity
        startAttackAnimation wkind vectorToTarget
        executeAttackByKind wkind
        startTimer Timer_Attack =<< getAttackCooldown
        self.ff#offensiveSlots %= dischargeRunicSlot
    where
    executeAttackByKind wkind = do
        power <- getAttackPower
        delay <- getAttackDelay
        let tid = targetEntity^.entityId
        addDelayedAction delay $ case wkind of
            WeaponKind_Slashing   -> DelayedActionType_Attack tid power
            WeaponKind_Thrusting  -> DelayedActionType_Attack tid power
            WeaponKind_Projecting ->
                DelayedActionType_FireProjectile vectorToTarget tid power

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
    attackRange <- getAttackRange
    dist <- fromMaybe attackRange <$> distanceToEntity targetEntity
    canFire <- canFireProjectile wkind
    when (dist < attackRange && canFire) act
    where
    canFireProjectile WeaponKind_Projecting = isJust <$> selectProjectile
    canFireProjectile _ = return True

getAttackRange :: Update Agent Distance
getAttackRange = updateStats >> use (self.fullStats.ff#attackRange)

getAttackPower :: Update Agent AttackPower
getAttackPower = updateStats >> use (self.fullStats.ff#attack)

getDefence :: Update Agent Defence
getDefence = updateStats >> use (self.fullStats.ff#defence)

getAttackDelay :: Update x Duration
getAttackDelay = return $ timeInSeconds 0.7

getAttackCooldown :: Update Agent Duration
getAttackCooldown = fromMaybe defaultCooldown
    <$> preuse (self.agentType.ff#unitType.traverse.ff#attackSpeed)
    where
    defaultCooldown = timeInSeconds 1.2

integrateLocationWhenWalking :: Update Agent ()
integrateLocationWhenWalking = do
    k <- use $ self.animationState.current.kind
    when (k == Animation.Walk) integrateLocation

getAggroTarget :: Update Agent (Maybe EntityWithId)
getAggroTarget = do
    trange <- use $ self.agentType.ff#autoTargetRange
    ds <- queryInRange EntityKind_Dynamic trange
    loc <- use $ self.location
    ht <- use $ self.agentType.ff#hostileTowards
    sid <- useSelfId
    let hs = sortWith (distanceTo loc) $ filter (shouldTarget ht sid) ds
    return $ viaNonEmpty head hs
    where
    shouldTarget ht sid x = x^.entityId /= sid && isHostileTo ht x
    distanceTo loc e = fromMaybe (1/0) $
        (distance (loc^._Wrapped) . view _Wrapped <$> e^.entity.oracleLocation)

autoTarget :: Update Agent ()
autoTarget = autoTargetWith $ \_ _ -> return ()

autoTargetMark :: Update Agent ()
autoTargetMark = autoTargetWith $ \currentTarget newTarget -> do
    mapM_ (flip addAction EntityAction_SelfUnmarkAsTarget) currentTarget
    mapM_ (flip addAction EntityAction_SelfMarkAsTarget) newTarget

autoTargetWith
    :: (Maybe EntityId -> Maybe EntityId -> Update Agent ())
    -> Update Agent ()
autoTargetWith func = do
    newTarget <- fmap (view entityId) <$> getAggroTarget
    currentTarget <- use $ self.target
    when (newTarget /= currentTarget) $
      whenM (isCloserBy 0.1 newTarget currentTarget) $ do
        self.target .= newTarget
        func currentTarget newTarget

    where
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
    EntityAction_AddLoadout    l -> mapM_ addLoadoutEntry l
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
            spawnPassive c $ def
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

