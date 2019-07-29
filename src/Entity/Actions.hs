module Entity.Actions
    ( EntityAction (..)
    -- ActOn Actions
    , setMoveVector
    , distanceToEntity
    , toggleDebugFlag
    , handleOnUpdate

    -- Update Actions
    , integrateLocation
    , vectorToEntity
    , moveTowards, orientTowards
    , selectAnimation
    , updateActiveAnimation
    , updateAnimationState
    , addEffect
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
    , renderTargetMark

    -- Queries
    , queryInRange, queryInRadius
    , queryById
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
import Types.Entity.Timer
import Types.Entity.Passive
import Types.Entity.Projectile
import Types.Entity.Appearance
import Types.Entity.Agent
import Types.Entity.Animation (AnimationState, AnimationKind)
import Types.Entity.Reactivity (ReactivCategory)
import Types.Entity.Effect (EffectKind)
import qualified Entity.Animation as Animation
import Entity.Utils
import Types.EntityIndex (EntityIndexTag(..))
import qualified Diagrams.TwoD.Transform as T
import qualified EntityIndex
import qualified Equipment
import Types.Equipment
import Equipment (Equipment, contentList)
import qualified Entity.Timer as Timer

import Data.Hashable (hash)
import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color
import ResourceManager (Resources, renderSprite)
import qualified Data.Collider as Collider
import qualified Data.Collider.Types as Collider

--------------------------------------------------------------------------------
-- ActOn Actions

setMoveVector
    :: HasVelocity s Velocity
    => HasMaxSpeed s Speed
    => V2D -> s -> s
setMoveVector moveVector s = s
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
integrateLocation = do
    vel <- use (self.velocity)
    self.location %= upd defaultDelta vel
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
moveTowards e = whenJustM (vectorToEntity e) $ \v -> self %= setMoveVector v

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

addEffect
    :: HasLocation s Location
    => EffectKind -> Update s ()
addEffect k = do
    cei <- queryByTag EntityIndexTag_Camera
    whenJust (view (entity.oracleLocation) =<< cei) $ \cloc -> do
        loc <- use $ self.location
        when (isWithinDistance maxEffectSpawnDistance loc cloc) $
            let st = SpawnEntity_Effect loc k
            in addWorldAction $ WorldAction_SpawnEntity st def

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
    :: HasTimer s Timer
    => Update s ()
updateTimer = self.timer %= Timer.update defaultDelta

startTimer
    :: HasTimer s Timer
    => TimerType -> Duration -> Update s ()
startTimer tt d = self.timer %= Timer.start tt d

checkTimeUp
    :: HasTimer s Timer
    => TimerType -> Update s Bool
checkTimeUp tt = do
    d <- uses (self.timer) (Timer.lookup tt)
    return (d <= 0)

separateCollision
    :: HasLocation s Location
    => HasCollisionShape s (Maybe CollisionShape)
    => Update s ()
separateCollision = do
    ss <- queryInRange EntityKind_Passive maxCollisionBBoxSize
    mc <- use $ self.collisionShape
    x <- use self
    let mlc = locate x <$> mc
    let ms = ss^..traverse.entity.oracleCollisionShape
    let svs = mapMaybe (mCol mlc) ms
    whenJust (viaNonEmpty head svs) $ \v -> self.location._Wrapped -= v
    where
    maxCollisionBBoxSize = Distance 4
    mCol ma mb = do
        a <- ma
        b <- mb
        Collider.collide a b

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
    => [(EntityId, Maybe EquipmentSlot)] -> Update x ()
addItems is = prepItems
    >>= equipItems
    >>= stuffIntoContainers
    >>= mapM_ dropItem
    >> flagUpdate UpdateOnce_Equipment
    where
    prepItems = catMaybes <$> mapM (\(e, ms) -> fmap (,ms) <$> queryById e) is

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
    :: HasEquipment x Equipment
    => EntityWithId
    -> [EntityWithId]
    -> Update x [EntityWithId]
stuffInto eq is = do
    let alwd = eq^.entity.oraclePassiveType.*.containerType.*.allowKinds
    let (allowedItems, otherItems) = splitAllowedItems alwd is
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

splitAllowedItems
    :: Set PassiveKind
    -> [EntityWithId]
    -> ([EntityWithId], [EntityWithId])
splitAllowedItems k = List.partition properKind
    where
    properKind x = let ks = x^?entity.oraclePassiveType.traverse.passiveKind
        in fmap (not . Set.null . Set.intersection k) ks == Just True

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
useItem e = addAction e . EntityAction_UseAction n =<< use (context.selfId)
    where n = UseActionName "Use"

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

renderCollisionShape :: Maybe CollisionShape -> RenderAction
renderCollisionShape cs = monoidJust cs $ \case
    Collider.Circle d -> renderShape $ def
        & shapeType .~ SimpleCircle
        & color     .~ Color.withOpacity Color.red 0.3
        & T.scale     (d^.radius)
        & translate (d^.center._Wrapped)
        & zindex    .~ 10000

renderTargetMark :: RenderAction
renderTargetMark = renderShape $ def
    & shapeType .~ SimpleCircle
    & color     .~ Color.withOpacity Color.red 0.3
    & T.scale  0.4
    & T.scaleY 0.7

--------------------------------------------------------------------------------
-- Queries

queryInRange
    :: HasLocation x Location
    => EntityKind -> Distance -> Update x [EntityWithId]
queryInRange k (Distance d) = do
    Location l <- use $ self.location
    let rng = mkBBoxCenter l (pure $ d*2)
    EntityIndex.lookupInRange k rng =<< use (context.entities)

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

