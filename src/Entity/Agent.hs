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
import Types.Entity.Passive
import Types.Entity.Animation
import Types.Equipment

import Entity
import Entity.HasField
import Entity.Utils
import Entity.Actions

import ResourceManager (Resources, lookupAnimation)
import qualified Equipment
import qualified Collider

import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color

import qualified Entity.Animation as Animation

--------------------------------------------------------------------------------

actOn :: Agent -> EntityAction -> Agent
actOn x a = x & case a of
    -- Handle here:
    EntityAction_ToggleDebug       f -> toggleDebugFlag f
    EntityAction_RunAnimation      k -> setAnimationKind k
    EntityAction_MoveTo            l -> setMoveTo l
    EntityAction_SetMoveVector     v -> setMoveVector v
    EntityAction_SelfHeal          h -> selfHeal h
    EntityAction_SetValue          v -> handleSetValue v
    EntityAction_SwapWeapon          -> swapWeapon
    -- Handle on update:
    EntityAction_PlayerAction     {} -> handleOnUpdate a

    EntityAction_AddItem          {} -> handleOnUpdate a
    EntityAction_RemoveItem       {} -> handleOnUpdate a

    EntityAction_ExecuteAttack    {} -> handleOnUpdate a
    EntityAction_SelfAttacked     {} -> handleOnUpdate a
    EntityAction_UseItem          {} -> handleOnUpdate a

    EntityAction_AddLoadout       {} -> handleOnUpdate a
    EntityAction_Interact         {} -> handleOnUpdate a
    _ -> id
    where
    setMoveTo l
        = set moveTo (Just l)
        . set target Nothing

    setMoveVector v
        = set moveTo Nothing
        . setMoveVelocity v

    selfHeal h _ = x & health %~
        (\ch -> min (x^.fullStats.maxHealth) (ch+h))

    setAnimationKind k _ = x
        & animationState.current.kind .~ k
        & animationState.current.era  .~ 0
        & animationState.progression  .~ Animation.defaultTransition

    handleSetValue ev _ = case ev of
        EntityValue_Location     v -> x & location .~ v
        EntityValue_Direction    d -> x & animationState.current.direction .~ d
        EntityValue_Animation    _ -> x
        EntityValue_SetStatus    s -> x & status %~ Set.insert s
        EntityValue_UnsetStatus  s -> x & status %~ Set.delete s

    swapWeapon = over equipment f
        where
        f e = Equipment.alter (const sw) EquipmentSlot_PrimaryWeapon
            . Equipment.alter (const pw) EquipmentSlot_SecondaryWeapon
            . Equipment.alter (const so) EquipmentSlot_PrimaryOther
            $ Equipment.alter (const po) EquipmentSlot_SecondaryOther e
            where
            pw = Equipment.lookupSlot EquipmentSlot_PrimaryWeapon   e
            sw = Equipment.lookupSlot EquipmentSlot_SecondaryWeapon e
            po = Equipment.lookupSlot EquipmentSlot_PrimaryOther    e
            so = Equipment.lookupSlot EquipmentSlot_SecondaryOther  e

-- Only update if close enougth to camera.
limitUpdate :: Update Agent () -> Update Agent ()
limitUpdate doUpdate = do
    let maxAnimateDistance = distanceInMeters 18
    loc <- use $ self.location
    cei <- queryByTag EntityIndexTag_Camera
    whenJust (view (entity.oracleLocation) =<< cei) $ \cloc -> do
        let dist = calcDistance loc cloc
        when (dist < maxAnimateDistance) $ do
            updateActiveAnimation
            pursueRange <- fromMaybe (1/0)
                <$> preuse (self.agentType.unitType.traverse.ff#pursueRange)
            when (dist < pursueRange) doUpdate

update :: Agent -> EntityContext -> Q (Maybe Agent, [DirectedAction])
update x ctx = runUpdate x ctx $ do
    limitUpdate $ do
        decideAction
        updatePlayerStatus -- This is a bit... not ideal.
        updateTimer
        updateDelayedActions
        integrateLocationWhenWalking
        updateMoveTo
        separateCollision

        allMatch _EntityAction_AddItem (addItems notify . toList)
        mapM_ processAction =<< use (self.processOnUpdate)
        mapM_ processUpdateOnce =<< use (self.updateOnce)

    self.processOnUpdate .= mempty
    where
    notify = whenM ((AgentKind_Player ==) <$> useAgentKind) $
        systemMessage "Not enough space in inventory."

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

    whenNoToMove = whenNothingM_ (use $ self.moveTo)
    enemyActions eunit = whenNoToMove $ do
        self.velocity .= 0
        autoTarget
        pursueOrAttackTarget eunit
        -- spreadOut

    npcActions = do
        unlessM (use $ self.ff#npcRegistered) $ do
            registerSelf
            self.ff#npcRegistered .= True
        whenNoToMove $ do
            self.velocity .= 0
            autoTarget
            whenJustM getTarget $ \targetEntity -> do
                attackRange <- getAttackRange
                dist <- fromMaybe (1/0) <$> distanceToEntity targetEntity
                when (dist < attackRange) $ do
                    orientTowards targetEntity
                    executeAttack

    registerSelf = whenJustM (use $ self.agentType.ff#scriptName) $ \sn ->
        addWorldAction . WorldAction_RegisterNPC sn =<< useSelfId

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
        dist <- fromMaybe (1/0) <$> distanceToEntity targetEntity
        if dist < attackRange
        then executeAttack -- attackTarget eunit targetEntity
        else if dist < pursueRange
            then orientTowards targetEntity >> moveTowards targetEntity
            else self.target .= Nothing

{-
    spreadOut = do
        let disperseRange = distanceInMeters 0.5
        es <- queryInRadius EntityKind_Dynamic disperseRange
        hostiles <- isHostileTo <$> use (self.agentType.ff#hostileTowards)
        let ts = filter (not . hostiles) es
        vs <- catMaybes <$> mapM vectorToEntity ts
        let v = sum $ map (negate . normalize) vs
        s <- use $ self.maxSpeed
        self.velocity += velocityFromSpeed v (s*0.3)
-}

runicActions :: Update Agent ()
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
        return $ makeComposeAnimation rs bns

makeComposeAnimation :: Resources -> [AnimationName] -> Animation
makeComposeAnimation rs = mconcat . mapMaybe (flip lookupAnimation rs)

updateStats :: Update Agent ()
updateStats = do
    let exs = [ EquipmentSlot_SecondaryWeapon, EquipmentSlot_SecondaryOther ]
    eis <- uses (self.equipment) (Equipment.excludeSlots ?? exs)
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

updatePlayerStatus :: Update Agent ()
updatePlayerStatus = whenM ((AgentKind_Player ==) <$> useAgentKind) $ do
    self.canAttackTarget <~ canExecuteAttack

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
    attack t p = addAction t . EntityAction_SelfAttacked p . Just =<< useSelfId
    fireProjectile v tid p = whenJustM selectProjectile $ \pid -> do
        sid <- useSelfId
        loc <- use $ self.location
        addAction pid $ EntityAction_SelfFiredAsProjectile $ FiredProjectileOpts
            { field_location    = loc
            , field_direction   = v
            , field_source      = Just sid
            , field_target      = tid
            , field_attackPower = p
            }

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
canExecuteAttack = do
    tu <- checkTimeUp Timer_Attack
    aa <- canAgentAttack
    mt <- fmap join . mapM queryById =<< use (self.target)
    wa <- maybe (return False) weaponCanAttack mt
    return $ and [tu, aa, wa]
    where
    canAgentAttack = useAgentKind >>= \x -> if x == AgentKind_Player
        then enougthRunicPoints
        else return True

    enougthRunicPoints = do
        rp <- use $ self.runicPoints._Wrapped
        ap <- Unwrapped <$> getAttackPower
        return (ap <= rp)

getWeaponKind :: Update Agent (Maybe WeaponKind)
getWeaponKind = do
    mei <- getEquippedItem EquipmentSlot_PrimaryWeapon
    return $ mei^?traverse.entity.oraclePassiveType.traverse.weaponKind.traverse

executeAttackAt :: EntityWithId -> V2 Float -> Update Agent ()
executeAttackAt t vectorToTarget = do
    wkind <- fromMaybe WeaponKind_Slashing <$> getWeaponKind
    orientTowards t
    startAttackAnimation wkind vectorToTarget
    power <- getAttackPower
    delay <- getAttackDelay
    executeAttackByKind delay power wkind
    startTimer Timer_Attack =<< getAttackCooldown
    self.runicPoints %= max 0 . subtract (Wrapped $ Unwrapped power)
    where
    tid = t^.entityId
    executeAttackByKind delay power = addDelayedAction delay . \case
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

weaponCanAttack :: HasEntity e Entity => e -> Update Agent Bool
weaponCanAttack targetEntity = do
    wkind <- fromMaybe WeaponKind_Slashing <$> getWeaponKind
    attackRange <- getAttackRange
    dist <- fromMaybe (1/0) <$> distanceToEntity targetEntity
    canFire <- canFireProjectile wkind
    return $ dist < attackRange && canFire
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
    loc <- use $ self.location
    cs <- use $ self.agentType.collisionShape
    self.collisionShape .= (Collider.locateShape loc <$> cs)

updateMoveTo :: Update Agent ()
updateMoveTo = whenJustM (use $ self.moveTo) $ \moveLoc -> do
    loc <- use $ self.location
    let dir = Unwrapped moveLoc - Unwrapped loc
    let dis = norm dir
    self %= setMoveVelocity dir
    nloc <- nextFrameLocation
    let ndis = norm $ Unwrapped moveLoc - Unwrapped nloc
    if dis <= ndis
    then do
        self.location .= moveLoc
        self.velocity .= 0
        self.moveTo   .= Nothing
    else do
        self.animationState.current.direction %= Animation.vecToDir dir

getAggroTarget :: Update Agent (Maybe EntityWithId)
getAggroTarget = do
    trange <- use $ self.agentType.ff#autoTargetRange
    ds <- queryInRadius EntityKind_Dynamic trange
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
autoTarget = autoTargetWith False $ \_ -> return ()

autoTargetMark :: Update Agent ()
autoTargetMark = autoTargetWith True $ addWorldAction . WorldAction_MarkTarget

autoTargetWith :: Bool -> (Maybe EntityId -> Update Agent ()) -> Update Agent ()
autoTargetWith disengage func = do
    newTarget <- fmap (view entityId) <$> getAggroTarget
    currentTarget <- use $ self.target
    when (newTarget /= currentTarget && (disengage || isJust newTarget)) $
      whenM (isCloserBy 0.1 newTarget currentTarget) $ do
        self.target .= newTarget
        func newTarget

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
    EntityAction_UseItem        i -> useItem i
    EntityAction_SelfAttacked d a -> procAttacked d a
    EntityAction_RemoveItem     i -> removeItem i
    EntityAction_AddLoadout     l -> mapM_ addLoadoutEntry l
    EntityAction_PlayerAction   a -> processPlayerAction a
    EntityAction_Interact     n e -> interact e n
    _ -> return ()

    where
    interact t = mapM_ (performInteractionEffect t) <=< getInteraction
    getInteraction = \case
        Just a  -> use $ self.agentType.ff#interactions.at(a).traverse
        Nothing -> do
            ma <- use $ self.agentType.ff#primaryInteraction
            case ma of
                Nothing -> return []
                Just a  -> use $ self.agentType.ff#interactions.at(a).traverse

performInteractionEffect :: EntityId -> InteractionEffect -> Update Agent ()
performInteractionEffect t = \case
    InteractionEffect_TransformInto _ -> return ()
    InteractionEffect_InspectContent  -> return ()
    InteractionEffect_DeleteSelf      -> return ()
    InteractionEffect_Heal          _ -> return ()
    InteractionEffect_TalkTo          -> talkTo
    where
    talkTo = do
        whenJustM (queryById t) orientTowards
        addWorldAction . WorldAction_StartDialog =<< useSelfId

processPlayerAction :: PlayerAction -> Update Agent ()
processPlayerAction = \case
    PlayerAction_UpdateRune  p s -> updateCurrentRune s p
    PlayerAction_SetAttackMode m -> setAttackMode m
    where
    updateCurrentRune s = when s . addRunicPoints

    addRunicPoints p = do
        mx <- use $ self.ff#maxRunicPoints
        self.runicPoints %= min mx . (p+)

    setAttackMode = assign (self.ff#attackMode)

systemMessage :: Text -> Update x ()
systemMessage = addWorldAction . WorldAction_Message . Message_Info

removeItem :: EntityId -> Update Agent ()
removeItem i = do
    self.equipment %= Equipment.deleteId i
    flagUpdate UpdateOnce_Equipment

procAttacked :: AttackPower -> Maybe EntityId -> Update Agent ()
procAttacked attackPower mAttackerId = do
    whenJust mAttackerId $ assign (self.target) . Just
    applyAttackDamage
    whenM shouldDie doDie
    where
    applyAttackDamage = do
        rd <- calcRunicDefence
        let ap = Health . max 0 $ (Unwrapped attackPower) - rd
        self.health %= max 0 . subtract ap
        loc <- use $ self.location
        off <- use $ self.agentType.renderOffset
        let rloc = over _Wrapped (+ fromMaybe 0 off) loc
        addHitEffect rloc ap

    calcRunicDefence = useAgentKind >>= \case
        AgentKind_Player -> do
            defence <- Unwrapped <$> getDefence
            let attack = Unwrapped attackPower
            let mda = min defence attack
            rp <- uses (self.runicPoints) Unwrapped
            self.runicPoints._Wrapped %= max 0 . subtract mda
            return $ min rp mda
        _ -> Unwrapped <$> getDefence

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
    [ renderDebug
    , addRenderOffset renderAnim
    ]
    where
    renderDebug = renderComposition $ localDebug <> globalDebug
    renderAnim  = Animation.renderAnimation (x^.animationState) (x^.animation)

    addRenderOffset = fromMaybe id $ fmap translate $ x^.agentType.renderOffset

    localDebug = map snd
        $ filter (\(f, _) -> x^.debugFlags.f)
        [ (drawPickupRange, renderPickupRange)
        ]

    globalDebug = map snd
        $ filter (\(f, _) -> Set.member f $ ctx^.debugFlags)
        [ (DebugFlag_ShowCollisionShapes, renderCollisionShape cs)
        ]

    cs = x^.agentType.collisionShape

    rangeScale = defaultPickupRange^._Wrapped
    renderPickupRange = T.scale rangeScale $ renderShape $ def
        & shapeType   .~ SimpleCircle
        & color       .~ Color.withOpacity Color.red 0.3

oracle :: Agent -> EntityQuery a -> Maybe a
oracle x = \case
    EntityQuery_Name               -> Just agentName
    EntityQuery_DisplayName        -> Just prettyAgentName
    EntityQuery_Location           -> Just $ x^.location
    EntityQuery_Equipment          -> Just $ x^.equipment
    EntityQuery_AgentType          -> Just $ x^.agentType
    EntityQuery_CollisionShape     -> x^.collisionShape
    EntityQuery_CollisionBits      -> Just $ x^.collisionBits
    EntityQuery_StandingWeight     -> Just $ x^.standingWeight
    EntityQuery_Reactivity         -> agentReactivity
    EntityQuery_Status             -> Just $ x^.status
    EntityQuery_PlayerStatus       -> Just $ upcast x
    EntityQuery_Interactions       -> Just $ Map.keys $ x^.agentType.interactions
    EntityQuery_PrimaryInteraction -> x^.agentType.primaryInteraction
    EntityQuery_LabelOffset        -> x^.agentType.labelOffset
    _                              -> Nothing
    where
    agentName = x^.agentType.name._Wrapped
    prettyAgentName = fromMaybe (prettyName agentName) $ x^.agentType.displayName
    agentReactivity
        | Set.member EntityStatus_Ignore (x^.status) = Nothing
        | otherwise = Just $ x^.agentType.reactivity

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
    & animation         .~ makeComposeAnimation rs (p^.bodyAnimation)
    & health            .~ p^.stats.maxHealth
    & baseStats         .~ p^.stats
    & agentType         .~ p

    & initPlayer
    where
    initPlayer
        | isPlayer  = set (ff#maxRunicPoints) 20
        | otherwise = id
    isPlayer = p^.agentKind == AgentKind_Player

