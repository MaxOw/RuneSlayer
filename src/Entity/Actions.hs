module Entity.Actions
    ( EntityAction (..)
    -- ActOn Actions
    , setMoveVector
    , toggleDebugFlag
    , handleOnUpdate

    -- Update Actions
    , integrateLocation
    , updateAnimation
    , updateEffects, addEffect
    , separateCollision
    , addItems
    , dropAllItems
    , pickUpInformOwner
    , dropItem, dropItemAction
    , getItemsToAdd
    , makeDropItem, splitItemKind

    -- Render Actions
    , maybeLocate, locate
    , withZIndex
    -- , renderSprite
    , renderAppearance
    , renderBBox
    , renderCollisionShape
    , renderTargetMark
    , renderEffects

    -- Queries
    -- , queryStaticInRange
    , queryInRadius
    , queryById
    , shouldDie
    , useSelfId

    -- Utils
    , addAction, addWorldAction
    , anyMatch, whenMatch
    , ifJustLocation
    ) where

import Delude
import qualified Data.Set as Set
import qualified Data.List as List
import Random.Utils
import Data.Hashable (hash)
import Engine.Common.Types (BBox, bboxToRect, mkBBoxCenter)
import Engine.Layout.Render (renderSimpleBox)
import Engine.Layout.Types (border)

import Types.Entity
import Types.Entity.ItemType
import Types.Entity.Appearance
import Types.Entity.Animation
    (AnimationState, EffectState, EffectKind, effectUpdate)
import qualified Entity.Animation as Animation
import Entity.Utils
import qualified EntityIndex
import qualified Equipment
import Types.Equipment
import Equipment (Equipment, contentList)

import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color
import ResourceManager (lookupSpriteName, Resources)
import Types.Sprite (pixelsPerUnit, SpriteName)
import qualified Data.Collider as Collider
import qualified Data.Collider.Types as Collider

--------------------------------------------------------------------------------
-- ActOn Actions

setMoveVector
    :: HasVelocity s Velocity
    => HasMaxSpeed s Speed
    => V2D -> s -> s
setMoveVector moveVector s = set velocity vel s
    where
    Speed speed = view maxSpeed s
    vel = velocityInMetersPerSecond $ normalize moveVector ^* speed

toggleDebugFlag :: HasDebugFlags x EntityDebugFlags => EntityDebugFlag -> x -> x
toggleDebugFlag = \case
    EntityDebugFlag_DrawPickupRange -> flipFlag drawPickupRange
    where
    flipFlag f = over (debugFlags.f) not

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
    upd (Time t) (Velocity v) (Location l) = Location $ l ^+^ (v^*t)

updateAnimation
    :: HasVelocity       s Velocity
    => HasAnimationState s AnimationState
    => Update s ()
updateAnimation = do
    vel <- use $ self.velocity._Wrapped
    a <- use $ self.animationState
    when (a^.current.kind == Animation.Walk) $
        if (norm vel == 0)
        then self.animationState.progression .= Animation.Stopped
        else do
            self.animationState.progression       .= Animation.Cycle
            self.animationState.current.direction %= Animation.vecToDir vel
    self.animationState %= Animation.update defaultDelta

    {-
    let Time t = defaultDelta
    d <- use $ self.animation.current.direction
    let animationSpeed = -- 1 --  0.01
            if d == Animation.North || d == Animation.South then 1.6 else 1.2
    let upd x = if x > 1 then x - 1 else x
    let eraChange = n * t * animationSpeed
    self.animation.current.direction %= Animation.vecToDir vel
    self.animation.current.era       %= upd . (+eraChange)
    when (n == 0) $
        self.animation.current.era .= 0
    -}

updateEffects
    :: HasEffects s [EffectState]
    => Time -> Update s ()
updateEffects delta = self.effects %= mapMaybe (\x -> (x^.effectUpdate) delta x)

addEffect
    :: HasEffects s [EffectState]
    => EffectKind -> Update s ()
addEffect k = case k of
    Animation.HitEffect _ -> self.effects %= ((Animation.makeEffect k hitUpdate):)
    where
    hitUpdate dt s = endByEra dt $ s
    endByEra dt s
        | ss^.era >= s^.duration = Nothing
        | otherwise              = Just ss
        where ss = s & era +~ (dt^._Wrapped)

separateCollision
    :: HasLocation s Location
    => HasCollisionShape s (Maybe CollisionShape)
    => Update s ()
separateCollision = do
    Location l <- use $ self.location
    let range = mkBBoxCenter l maxCollisionBBoxSize
    ss <- queryStaticInRange range
    mc <- use $ self.collisionShape
    x <- use self
    let mlc = locate x <$> mc
    let ms = ss^..traverse.entity.oracle.collisionShape
    let svs = mapMaybe (mCol mlc) ms
    whenJust (viaNonEmpty head svs) $ \v -> self.location._Wrapped -= v
    where
    maxCollisionBBoxSize = pure 4
    mCol ma mb = do
        a <- ma
        b <- mb
        Collider.collide a b

--------------------------------------------------------------------------------

dropAllItems
    :: HasEquipment       x Equipment
    => HasLocation        x Location
    => Update             x ()
dropAllItems = do
    is <- use $ self.equipment.to contentList
    self.equipment %= Equipment.deleteAll
    mapM_ dropItemAction is

--------------------------------------------------------------------------------

purePickUpInformOwner
    :: HasOwner x (Maybe EntityId)
    => x -> EntityContext -> [DirectedAction]
purePickUpInformOwner x ctx = case x^.owner of
    Just ownerId -> [directAtEntity ownerId act]
    _            -> []
    where
    act = EntityAction_AddItem        $ ctx^.selfId

pickUpInformOwner
    :: HasOwner x (Maybe EntityId)
    => Update x ()
pickUpInformOwner = do
    newActions <- purePickUpInformOwner <$> use self <*> use context
    actions <>= fromList newActions

--------------------------------------------------------------------------------

addItems
    :: HasProcessOnUpdate x [EntityAction]
    => HasEquipment       x Equipment
    => HasLocation        x Location
    => Update x ()
addItems = do
    os <- equipItems =<< getItemsToAdd
    let (sit, oit) = splitItemKind ItemKind_SmallItem os
    equippedBackpack >>= \case
        Just ct -> mapM_ (addAction ct . EntityAction_AddItem . view entityId) sit
        Nothing -> mapM_ (dropItemAction . view entityId) sit
    mapM_ (dropItemAction . view entityId) oit

useSelfId :: Update x EntityId
useSelfId = use $ context.selfId

equippedBackpack
    :: HasEquipment x Equipment
    => Update x (Maybe EntityId)
equippedBackpack = do
    Equipment.lookupSlot EquipmentSlot_Backpack <$> use (self.equipment)


{-
    -- if there is no container equiped find one
    -- if found equip it and drop the rest
    -- (newSelf, newActions) <- pureAddItems <$> use context <*> use self
    -- self .= newSelf
    -- actions <>= fromList newActions
    -- return ()
    case mb of
        Just eb -> return (Just eb, es)
        Nothing -> do
            let (cs, os) = splitItemKind ItemKind_Container es
            case map (view entityId) cs of
                []     -> return (Nothing, [])
                (a:as) -> do
                    mapM_ dropItemAction as
                    return (Just a, os)
-}

splitItemKind :: ItemKind -> [EntityWithId] -> ([EntityWithId], [EntityWithId])
splitItemKind k = List.partition properKind
    where
    properKind x = x^.entity.oracle.itemKind == Just k

equipItems
    :: HasEquipment x Equipment
    => [EntityWithId] -> Update x [EntityWithId]
equipItems eis = do
    p <- use self
    let (newSelf, os) = go p (emptySlots p) [] eis
    self .= newSelf
    return os
    where
    go x Empty bs    es  = (x, bs <> es)
    go x  _    bs    []  = (x, bs)
    go x ss    bs (e:es) = case fitEntity (e^.entity) ss of
        Just t  -> go (equipItem (e^.entityId) t x) (Set.delete t ss) bs es
        Nothing -> go x ss (e:bs) es

    emptySlots = Equipment.emptySlots . view equipment
    fitEntity e = find $ flip Set.member (e^.oracle.fittingSlots)
    equipItem e eslot = over equipment $ Equipment.insert eslot e

getItemsToAdd
    :: HasProcessOnUpdate x [EntityAction]
    => Update x [EntityWithId]
getItemsToAdd = do
    ps <- mapMaybe g <$> use (self.processOnUpdate)
    eix <- use $ context.entities
    catMaybes <$> mapM (flip EntityIndex.lookupById eix) ps
    where
    g (EntityAction_AddItem        i) = Just i
    g _                               = Nothing

--------------------------------------------------------------------------------

dropItem
    :: HasEquipment       x Equipment
    => HasLocation        x Location
    => EntityId -> Update x ()
dropItem i = do
    eq <- use $ self.equipment
    if (Equipment.hasId i eq)
    then do
        self.equipment %= Equipment.deleteId i
        dropItemAction i
    else whenJustM equippedBackpack $ \b -> do
        addAction b $ EntityAction_DropItem i

--------------------------------------------------------------------------------

addAction :: HasEntityId e EntityId => e -> EntityAction -> Update x ()
addAction e x = actions %= (:> directAtEntity (e^.entityId) x)

addWorldAction :: WorldAction -> Update x ()
addWorldAction a = actions %= (:> directAtWorld a)

dropItemAction
    :: HasLocation x Location
    => EntityId -> Update x ()
dropItemAction eid = do
    loc <- use $ self.location
    fct <- use $ context.frameCount
    addAction eid $ makeDropItem fct loc eid

makeDropItem :: Word32 -> Location -> EntityId -> EntityAction
makeDropItem rnd loc eid = EntityAction_SelfDroppedAt dloc
    where
    rgid = fromIntegral $ hash eid
    dloc = over _Wrapped (\v -> v + genDropOffset [rnd + rgid]) loc

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

renderSpriteN :: HasResources c Resources => c -> SpriteName -> RenderAction
renderSpriteN ctx r = case lookupSpriteName r $ ctx^.resources of
    Nothing      -> renderShape shape
    Just (s,img) -> renderImg img & sscale (s^.pixelsPerUnit)
    where
    sscale = maybe id (\s -> scale $ 1/(fromIntegral s))
    shape = def
        & shapeType   .~ SimpleSquare
        & color       .~ Color.opaque Color.gray

renderAppearance :: HasResources c Resources => c -> Appearance -> RenderAction
renderAppearance ctx = \case
    Appearance_Sprite         r -> renderSpriteN ctx r
    Appearance_Translate    t a -> translate t $ renderAppearance ctx a
    Appearance_Compose as -> renderComposition $ map (renderAppearance ctx) as

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
        & scale     (d^.Collider.radius)
        & translate (d^.Collider.center._Wrapped)
        & zindex    .~ 10000

renderTargetMark :: RenderAction
renderTargetMark = renderShape $ def
    & shapeType .~ SimpleCircle
    & color     .~ Color.withOpacity Color.red 0.3
    & scale  0.4
    & scaleY 0.7

renderEffects :: HasEffects x [EffectState] => x -> RenderAction
renderEffects = renderComposition . map Animation.renderEffect . view effects

--------------------------------------------------------------------------------
-- Queries

queryStaticInRange :: RangeBBox -> Update s [EntityWithId]
queryStaticInRange rng =
    EntityIndex.lookupInRange EntityKind_Static rng =<< use (context.entities)

queryInRadius
    :: HasLocation x Location
    => EntityKind -> Distance -> Update x [EntityWithId]
queryInRadius k (Distance d) = do
    Location l <- use $ self.location
    let rng = mkBBoxCenter l (pure $ d*2)
    EntityIndex.lookupInRange k rng =<< use (context.entities)

queryById :: EntityId -> Update s (Maybe EntityWithId)
queryById eid = EntityIndex.lookupById eid =<< use (context.entities)

shouldDie :: HasHealth x Health => Update x Bool
shouldDie = uses (self.health._Wrapped) (<=0)

--------------------------------------------------------------------------------
-- Utils

anyMatch
    :: HasProcessOnUpdate x [EntityAction]
    => APrism' EntityAction y
    -> (NonEmpty y -> Update x ())
    -> Update x ()
anyMatch p act = do
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
genDropOffset s = randomFromSeed s $ \gen -> do
    r <- uniformRange (0.5, 1.5) gen -- make this into global constants
    d <- randomDirection gen
    return (r*^d)

