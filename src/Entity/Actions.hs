module Entity.Actions
    ( EntityAction (..)
    -- ActOn Actions
    , setMoveVelocity
    , distanceToEntity
    , toggleDebugFlag
    , handleOnUpdate

    -- Update Actions
    , integrateLocation, nextFrameLocation
    , vectorToEntity
    , moveTowards, orientTowards
    , selectAnimation
    , updateActiveAnimation
    , updateAnimationState
    , addHitEffect
    , spawnProjectile, spawnPassive
    , addLoadoutEntry
    , updateTimer, startTimer, checkTimeUp
    , separateCollision
    , addItems
    , dropAllItems
    , dropItem
    , passItemTo
    , useItem
    , splitAllowedItems
    , getEquippedItem
    , flagUpdate
    , getRootLocation

    -- Render Actions
    , maybeLocate, locate
    , withZIndex
    -- , renderSprite
    , renderAppearance
    , renderBBox
    , renderCollisionShape

    -- Queries
    , queryInRange, queryInRadius
    , queryById, queryByTag -- , queryPlayer
    , shouldDie
    , useSelfId

    -- Utils
    , addAction, addWorldAction
    , firstMatch, allMatch, whenMatch
    , ifJustLocation
    , isHostileTo
    , distanceBetween
    , velocityFromSpeed
    , genDropOffset
    ) where

import Delude
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Random.Utils
import Engine.Common.Types (BBox, bboxToRect, mkBBoxCenter)
import Engine.Layout.Render (renderSimpleBox)

import Entity
import Types.Entity.Passive
import Types.Entity.Projectile
import Types.Entity.Appearance
import Types.Entity.Agent
import Types.Entity.Animation (AnimationState, AnimationKind)
import Types.Entity.Reactivity (ReactivCategory)
import qualified Entity.Animation as Animation
import Entity.Utils
import Entity.HasField
import Types.EntityIndex (EntityIndexTag(..))
import qualified EntityIndex
import qualified Equipment
import Types.Equipment
import Equipment (Equipment, contentList)
import qualified Data.Timer as Timer
import Data.Timer (Timer)

import Data.Hashable (hash)
import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color
import ResourceManager (Resources, renderSprite)
import Types.Collider (Shape, CollideWith)
import Data.BitSet (BitSet32)
import qualified Data.BitSet as BitSet
import qualified Collider
-- import qualified Data.Collider.Types as Collider

--------------------------------------------------------------------------------
-- ActOn Actions

setMoveVelocity
    :: HasVelocity s Velocity
    => HasMaxSpeed s Speed
    => V2D -> s -> s
setMoveVelocity moveVector s = s
    & velocity .~ velocityFromSpeed moveVector (s^.maxSpeed)

distanceToEntity
    :: HasLocation s Location
    => HasEntity e Entity
    => e -> Update s (Maybe Distance)
distanceToEntity (view entity -> e) = do
    loc <- use $ self.location._Wrapped
    let tmloc = e^.oracleLocation
    return (Distance . distance loc . view _Wrapped <$> tmloc)

toggleDebugFlag :: HasDebugFlags x EntityDebugFlags => EntityDebugFlag -> x -> x
toggleDebugFlag = \case
    EntityDebugFlag_DrawPickupRange -> flipFlag drawPickupRange
    where
    flipFlag :: HasDebugFlags x f => Lens' f Bool -> (x -> x)
    flipFlag f = over (debugFlags . f) not

handleOnUpdate :: HasProcessOnUpdate s [a] => a -> s -> s
handleOnUpdate a = over processOnUpdate (a:)

--------------------------------------------------------------------------------
-- Update Actions

integrateLocation
    :: HasVelocity s Velocity
    => HasLocation s Location
    => Update s ()
integrateLocation = self.location <~ nextFrameLocation

nextFrameLocation
    :: HasVelocity s Velocity
    => HasLocation s Location
    => Update s Location
nextFrameLocation = do
    vel <- use (self.velocity)
    loc <- use (self.location)
    return $ upd defaultDelta vel loc
    where
    upd (Duration t) (Velocity v) (Location l) = Location $ l ^+^ (v^*t)


vectorToEntity
    :: HasLocation s Location
    => HasEntity e Entity
    => e -> Update s (Maybe V2D)
vectorToEntity (view entity -> e) = do
    loc <- use $ self.location
    return $ directionToTarget loc <$> e^.oracleLocation
    where
    directionToTarget (Location a) (Location b) = b - a

moveTowards
    :: HasLocation s Location
    => HasVelocity s Velocity
    => HasMaxSpeed s Speed
    => HasEntity e Entity
    => e -> Update s ()
moveTowards e = whenJustM (vectorToEntity e) $ \v -> self %= setMoveVelocity v

orientTowards
    :: HasAnimationState s AnimationState
    => HasLocation       s Location
    => HasEntity e Entity
    => e -> Update s ()
orientTowards e = whenJustM (vectorToEntity e) $ \v ->
    self.animationState.current.direction %= Animation.vecToDir v

selectAnimation
    :: HasAnimationState     s AnimationState
    => AnimationKind
    -> Update s ()
selectAnimation k = do
    self.animationState.current.kind .= k
    self.animationState.current.era  .= 0
    self.animationState.progression  .= Animation.defaultTransition

updateActiveAnimation
    :: HasVelocity           s Velocity
    => HasAnimationState     s AnimationState
    => HasAnimateWhenStopped s Bool
    => Update s ()
updateActiveAnimation = do
    vel <- use $ self.velocity._Wrapped
    a <- use $ self.animationState
    dontStop <- use $ self.animateWhenStopped
    when (a^.current.kind == Animation.Walk) $
        if (norm vel == 0 && not dontStop)
        then do
            self.animationState.progression .= Animation.Stopped 0
        else do
            self.animationState.progression       .= Animation.Cycle
            self.animationState.current.direction %= Animation.vecToDir vel
    updateAnimationState

updateAnimationState :: HasAnimationState s AnimationState => Update s ()
updateAnimationState = self.animationState %= Animation.update defaultDelta

addHitEffect :: Location -> Health -> Update s ()
addHitEffect loc hp = do
    cei <- queryByTag EntityIndexTag_Camera
    whenJust (view (entity.oracleLocation) =<< cei) $ \cloc -> do
        when (isWithinDistance maxEffectSpawnDistance loc cloc) $
            addWorldAction $ WorldAction_Message $ Message_HitEffect loc hp
            -- let st = SpawnEntity_Effect k
            -- in addWorldAction $ WorldAction_SpawnEntity st $ def
                -- & set actions [ EntityAction_SetValue $ EntityValue_Location loc ]

spawnProjectile :: Projectile -> Update s ()
spawnProjectile p =
    addWorldAction $ WorldAction_SpawnEntity (SpawnEntity_Projectile p) def

spawnPassive :: PassiveTypeName -> SpawnEntityOpts -> Update x ()
spawnPassive n = addWorldAction . WorldAction_SpawnEntity (SpawnEntity_Passive n)

addLoadoutEntry :: LoadoutEntry (Spawn PassiveTypeName EntityAction) -> Update x ()
addLoadoutEntry e = whenRndSelect (e^.ff#probability) (e^.ff#selection) $ \x -> do
    ct <- genCount (e^.ff#countRange)
    sid <- useSelfId
    replicateM_ ct $ spawnPassive (x^.name) $ def & set actions
        (EntityAction_SelfPassTo (Just sid) (e^.ff#slot) : x^.actions)
    where
    whenRndSelect mp s f = do
        seed <- getFrameIdSeed
        let p = fromMaybe (1/0) $ fmap unProbability mp
        let i = runRandom seed uniform
        when (i <= p) $ do
            let ss = map (over _1 unProbability . selToPair) s
            let ms = runRandom seed $ uniformSelectWeighted ss
            whenJust ms f
    selToPair s = (fromMaybe 1 $ s^.ff#probability, s^.ff#entry)
    genCount = \case
        Nothing -> return 1
        Just rn -> do
            let (Range mn mx) = fromIntegral <$> rn
            seed <- getFrameIdSeed
            return $ max 1 $ runRandom seed $ uniformRange (mn, mx)

getFrameIdSeed :: Update x Int
getFrameIdSeed = liftA2 (+)
    (fromIntegral <$> use (context.frameCount))
    (hash <$> useSelfId)

updateTimer
    :: HasTimer s (Timer a)
    => Update s ()
updateTimer = self.timer %= Timer.update defaultDelta

startTimer
    :: HasTimer s (Timer TimerType)
    => TimerType -> Duration -> Update s ()
startTimer tt d = self.timer %= Timer.start tt d

checkTimeUp
    :: HasTimer s (Timer TimerType)
    => TimerType -> Update s Bool
checkTimeUp tt = do
    d <- uses (self.timer) (Timer.lookup tt)
    return (d <= 0)

separateCollision
    :: HasLocation s Location
    => HasCollisionShape s (Maybe Shape)
    => HasCollisionBits  s (BitSet32 CollideWith)
    => HasStandingWeight s Weight
    => Update s ()
separateCollision = do
    sid <- useSelfId
    let excludeSelf = filter (\x -> x^.entityId /= sid)
    ds <- excludeSelf <$> queryInRange EntityKind_Dynamic maxCollisionBBoxSize
    ps <- queryInRange EntityKind_Passive maxCollisionBBoxSize
    let ss = ps <> ds
    stw <- uses (self.standingWeight) (max 1)
    mc <- use $ self.collisionShape
    cb <- use $ self.collisionBits
    let ms = ss^..traverse.entity.to shapeAndStandingWeight
    ssr <- map toTrip <$> queryStaticShapesInRange maxCollisionBBoxSize
    let svs = sortOn (Down . norm) $ take 5 $ mapMaybe (mCol cb stw mc) (ssr <> ms)
    whenJust (viaNonEmpty head svs) $ \v -> self.location._Wrapped -= v
    where
    shapeAndStandingWeight x = (,,)
        <$> x^.oracleCollisionShape
        <*> x^.oracleStandingWeight
        <*> x^.oracleCollisionBits
    toTrip s = Just (s^.ff#shape, Weight $ 1/0, s^.ff#mask)
    maxCollisionBBoxSize = Distance 2
    mCol acb asw ma mb = do
        a <- ma
        (b, bsw, bcb) <- mb
        let rel = Unwrapped $ min 1.0 $ bsw/asw
        if rel > 0.2 && not (BitSet.null $ BitSet.intersection acb bcb)
        then (rel*^) <$> Collider.separateShapes a b
        else Nothing

--------------------------------------------------------------------------------

dropAllItems
    :: HasEquipment       x Equipment
    => HasUpdateOnce      x (Set UpdateOnce)
    => Update             x ()
dropAllItems = do
    is <- use $ self.equipment.to contentList
    self.equipment %= Equipment.deleteAll
    mapM_ dropItem is
    flagUpdate UpdateOnce_Equipment

--------------------------------------------------------------------------------

addItems
    :: HasEquipment       x Equipment
    => HasLocation        x Location
    => HasUpdateOnce      x (Set UpdateOnce)
    => Update x ()
    -> [(EntityId, Maybe EquipmentSlot)]
    -> Update x ()
addItems notify is = prepItems
    >>= equipItems
    >>= stuffIntoContainers
    >>= \ds -> do
    mapM_ dropItem ds
    when (shouldNotify ds) notify
    flagUpdate UpdateOnce_Equipment
    where
    prepItems = catMaybes <$> mapM (\(e, ms) -> fmap (,ms) <$> queryById e) is
    shouldNotify [] = False
    shouldNotify ds = any (\x -> Set.member (x^.entityId) iss) ds
    iss = Set.fromList $ map (view _1) is

stuffIntoContainers
    :: HasEquipment x Equipment
    => [EntityWithId]
    -> Update x [EntityWithId]
stuffIntoContainers ls = do
    es <- fmap catMaybes . mapM queryById =<< uses (self.equipment) contentList
    foldStuff (filter isContainer es) ls
    where
    isContainer x
        = Set.member PassiveKind_Container
        $ x^.entity.oraclePassiveType.traverse.passiveKind
    foldStuff
        :: HasEquipment x Equipment
        => [EntityWithId]
        -> [EntityWithId]
        -> Update x [EntityWithId]
    foldStuff []     xs = return xs
    foldStuff (r:rs) xs = stuffInto r xs >>= foldStuff rs

-- TODO: combine this with fitIntoContainer function from Passive module
-- to account for volume (and weight in the future).
stuffInto
    :: EntityWithId
    -> [EntityWithId]
    -> Update x [EntityWithId]
stuffInto eq is = do
    let alwd = eq^.entity.oraclePassiveType.*.containerType.*.allowKinds
    let (allowedItems, otherItems) = splitAllowedItems (view entity) alwd is
    mapM_ (flip passItemTo eq) allowedItems
    return otherItems
    where
    infixr 9 .*.
    a.*.b = a.traverse.b

getEquippedItem
    :: HasEquipment x Equipment
    => EquipmentSlot
    -> Update x (Maybe EntityWithId)
getEquippedItem s = fmap join . mapM queryById
    =<< Equipment.lookupSlot s <$> use (self.equipment)

flagUpdate
    :: HasUpdateOnce x (Set UpdateOnce)
    => UpdateOnce
    -> Update x ()
flagUpdate f = self.ff#updateOnce %= Set.insert f

getRootLocation
    :: HasOwner x (Maybe EntityId)
    => HasLocation x (Maybe Location)
    => Update x (Maybe Location)
getRootLocation = do
    mloc <- use (self.location)
    mown <- use (self.owner)
    go mloc mown
    where
    -- This will loop if there is cycle in the owner chain
    -- That should never happen but it could be good to add a check for
    -- starting entityId
    go :: Maybe Location -> Maybe EntityId -> Update x (Maybe Location)
    go ml mi = case ml of
        Just lo -> return $ Just lo
        Nothing -> do
            me <- runMaybeT $ MaybeT . queryById =<< MaybeT (pure mi)
            let ol = me^?traverse.entity.oracleLocation.traverse
            let oo = me^?traverse.entity.oracleOwner.traverse
            go ol oo

useSelfId :: Update x EntityId
useSelfId = use $ context.selfId

splitAllowedItems :: (a -> Entity) -> Set PassiveKind -> [a] -> ([a], [a])
splitAllowedItems f k = List.partition properKind
    where
    properKind x = let ks = (f x)^?oraclePassiveType.traverse.passiveKind
        in fmap (not . Set.disjoint k) ks == Just True

equipItems
    :: HasEquipment x Equipment
    => [(EntityWithId, Maybe EquipmentSlot)]
    -> Update x [EntityWithId]
equipItems xs = do
    let (eis, ess) = partitionEithers $ map f xs
    pss <- catMaybes <$> mapM placeAtSlot ess
    p <- use self
    let (newSelf, os) = go p (emptySlots p) [] (pss <> eis)
    self .= newSelf
    return os
    where
    go :: HasEquipment x Equipment
       => x
       -> Set EquipmentSlot
       -> [EntityWithId]
       -> [EntityWithId]
       -> (x, [EntityWithId])
    go x Empty bs    es  = (x, bs <> es)
    go x  _    bs    []  = (x, bs)
    go x ss    bs (e:es) = case fitEntity (e^.entity) ss of
        Just t  -> go (equipItem (e^.entityId) t x) (Set.delete t ss) bs es
        Nothing -> go x ss (e:bs) es

    f (eid, Nothing) = Left eid
    f (eid, Just sl) = Right (eid, sl)

    placeAtSlot
        :: HasEquipment x Equipment
        => (EntityWithId, EquipmentSlot) -> Update x (Maybe EntityWithId)
    placeAtSlot (e, s) = uses (self.equipment) (Equipment.lookupSlot s) >>= \case
        Nothing -> do
            self.equipment %= Equipment.insert s eid
            return Nothing
        Just ce -> do
            self.equipment %= Equipment.deleteId ce
            self.equipment %= Equipment.insert s eid
            queryById ce
        where
        eid = e^.entityId

    emptySlots :: HasEquipment x Equipment => x -> Set EquipmentSlot
    emptySlots = Equipment.emptySlots . view equipment

    fitEntity e = find $ flip Set.member (fromMaybe mempty $ e^.oracleFittingSlots)

    equipItem :: HasEquipment x Equipment
        => EntityId -> EquipmentSlot -> (x -> x)
    equipItem e eslot = over equipment $ Equipment.insert eslot e

--------------------------------------------------------------------------------

dropItem :: HasEntityId e EntityId => e -> Update x ()
dropItem (view entityId -> e)
    = addAction e $ EntityAction_SelfPassTo Nothing Nothing

passItemTo
    :: HasEntityId a EntityId
    => HasEntityId b EntityId
    => a -> b -> Update x ()
passItemTo (view entityId -> a) (view entityId -> b)
    = addAction a $ EntityAction_SelfPassTo (Just b) Nothing

--------------------------------------------------------------------------------

addAction :: HasEntityId e EntityId => e -> EntityAction -> Update x ()
addAction e x = actions %= (:> directAtEntity (e^.entityId) x)

addWorldAction :: WorldAction -> Update x ()
addWorldAction a = actions %= (:> directAtWorld a)

useItem :: HasEntityId e EntityId => e -> Update x ()
useItem e = addAction e . EntityAction_Interact Nothing =<< use (context.selfId)

--------------------------------------------------------------------------------
-- Render Actions

maybeLocate
    :: HasLocation x (Maybe Location)
    => Transformable2D t
    => x -> (t -> t)
maybeLocate x = fromMaybe id $ x^?location.traverse._Wrapped.to translate

locate
    :: HasLocation x Location
    => Transformable2D t
    => x -> (t -> t)
locate x = x^.location._Wrapped.to translate

withZIndex :: GetZIndex x Word32
    => x -> (RenderAction -> RenderAction)
withZIndex x = setZIndexAtLeast (get_zindex x)

renderAppearance :: HasResources c Resources => c -> Appearance -> RenderAction
renderAppearance ctx (Appearance ls) = renderComposition $ map renderLocated ls
    where
    renderLocated (Located v s) = translate v $ renderSprite (ctx^.resources) s

renderBBox :: BBox Float -> RenderAction
renderBBox bb = renderSimpleBox $ def
    & size .~ (view size $ bboxToRect bb)
    & border.each.color .~ Color.opaque Color.gray
    & border.each.width .~ (1/32)

renderCollisionShape :: Maybe Shape -> RenderAction
renderCollisionShape Nothing  = mempty
renderCollisionShape (Just s) = Collider.renderShape s

--------------------------------------------------------------------------------
-- Queries

queryInRange
    :: HasLocation x Location
    => EntityKind -> Distance -> Update x [EntityWithId]
queryInRange k (Distance d) = do
    Location l <- use $ self.location
    let rng = mkBBoxCenter l (pure $ d*2)
    EntityIndex.lookupInRange k rng =<< use (context.entities)

queryStaticShapesInRange
    :: HasLocation x Location
    => Distance -> Update x [StaticShape]
queryStaticShapesInRange (Distance d) = do
    Location l <- use $ self.location
    let rng = mkBBoxCenter l (pure $ d*2)
    EntityIndex.lookupStaticShapesInRange rng =<< use (context.entities)

queryInRadius
    :: HasLocation x Location
    => EntityKind -> Distance -> Update x [EntityWithId]
queryInRadius k d = do
    loc <- use $ self.location
    catMaybes . map (qloc loc) <$> queryInRange k d
    where
    qloc l e = e^.entity.oracleLocation >>= \x ->
        if isWithinDistance d l x then Just e else Nothing

queryById :: EntityId -> Update s (Maybe EntityWithId)
queryById eid = EntityIndex.lookupById eid =<< use (context.entities)

queryByTag :: EntityIndexTag -> Update s (Maybe EntityWithId)
queryByTag t = EntityIndex.lookupByTag t =<< use (context.entities)

-- queryPlayer :: Update s (Maybe EntityWithId)
-- queryPlayer = queryByTag EntityIndexTag_Player

shouldDie :: HasHealth x Health => Update x Bool
shouldDie = uses (self.health._Wrapped) (<=0)

--------------------------------------------------------------------------------
-- Utils

firstMatch
    :: HasProcessOnUpdate x [EntityAction]
    => APrism' EntityAction y
    -> (y -> Update x ())
    -> Update x ()
firstMatch p act = do
    as <- uses (self.processOnUpdate) $ firstOf (traverse.clonePrism p)
    whenJust as act

allMatch
    :: HasProcessOnUpdate x [EntityAction]
    => APrism' EntityAction y
    -> (NonEmpty y -> Update x ())
    -> Update x ()
allMatch p act = do
    as <- uses (self.processOnUpdate) $ toListOf (traverse.clonePrism p)
    whenNotNull as act

whenMatch
    :: HasProcessOnUpdate x [EntityAction]
    => APrism' EntityAction y
    -> Update x ()
    -> Update x ()
whenMatch p act = do
    as <- use (self.processOnUpdate)
    when (any (isPrism p) as) act

ifJustLocation
    :: Monoid a
    => HasLocation x (Maybe Location)
    => x -> a -> a
ifJustLocation x a
    | isJust (x^.location) = a
    | otherwise            = mempty

genDropOffset :: [Word32] -> V2D
genDropOffset s = randomFromSeed s $ do
    r <- uniformRange (0.5, 1.5) -- make this into global constants
    d <- randomDirection
    return (r*^d)

isHostileTo :: HasEntity e Entity => Set ReactivCategory -> e -> Bool
isHostileTo hostileSet e = not $ Set.disjoint hostileSet
    (Map.keysSet $ fromMaybe mempty $ e^.entity.oracleReactivity)

distanceBetween :: Location -> Location -> Distance
distanceBetween (Location a) (Location b) = Distance $ distance a b

velocityFromSpeed :: V2D -> Speed -> Velocity
velocityFromSpeed v (Speed s) = velocityInMetersPerSecond $ normalize v ^* s

