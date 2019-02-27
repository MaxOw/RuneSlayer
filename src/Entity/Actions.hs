module Entity.Actions
    -- ActOn Actions
    ( setMoveVector
    , toggleDebugFlag
    , handleOnUpdate

    -- Update Actions
    , integrateLocation
    , separateCollision
    , addItems
    , dropAllItems
    , pickUpInformOwner
    , dropItem, dropItemAction
    , containerAddItems
    , containerDropItem

    -- Render Actions
    , maybeLocate, locate
    , withZIndex
    , renderSprite
    , renderAppearance
    , renderBBox

    -- Queries
    -- , queryStaticInRange

    -- Utils
    , anyMatch
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
import Entity.Utils
import qualified EntityIndex
import qualified Equipment
import Types.Equipment
import Equipment (Equipment, contentList)

import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color
import Resource (Resource)
import ResourceManager (lookupResource, ResourceMap)
import Types.ResourceManager (unitsPerPixel)

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

--------------------------------------------------------------------------------

separateCollision
    :: HasLocation s Location
    => Update s ()
separateCollision = do
    Location l <- use $ self.location
    let range = mkBBoxCenter l maxCollisionBBoxSize
    ss <- queryStaticInRange range
    return ()
    where
    maxCollisionBBoxSize = pure 4

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
    => x -> EntityContext -> [DirectedEntityAction]
purePickUpInformOwner x ctx = case x^.owner of
    Just ownerId -> [DirectedEntityAction ownerId act]
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

{-
sendItemsToContainer :: EntityId -> [EntityWithId] -> Update x ()
sendItemsToContainer eid es = do
    actions <>= fromList (map f es)
    where
    f x = DirectedEntityAction (x^.entityId) $ EntityAction_AddItem eid
-}

containerAddItems
    :: HasProcessOnUpdate x [EntityAction]
    => HasContent         x [EntityId]
    => HasContentVolume   x Volume
    => HasContainerType   x ContainerType
    => HasLocation        x (Maybe Location)
    => HasOwner           x (Maybe EntityId)
    => Update             x ()
containerAddItems = do
    os <- fitIntoContainer =<< getItemsToAdd
    mapM_ containerDropItem os

containerDropItem
    :: HasLocation x (Maybe Location)
    => HasContent  x [EntityId]
    => HasOwner    x (Maybe EntityId)
    => HasEntityId i EntityId
    => i -> Update x ()
containerDropItem e = use (self.location) >>= \case
    Nothing -> use (self.owner) >>= \mo -> whenJust mo $ \o -> do
        addAction o $ EntityAction_OwnerDropItem eid
        self.content %= List.delete eid
    Just lc -> use (context.frameCount) >>= \fct -> do
        addAction eid $ makeDropItem fct lc eid
        self.content %= List.delete eid
    where
    eid = e^.entityId

fitIntoContainer
    :: HasContent       x [EntityId]
    => HasContentVolume x Volume
    => HasContainerType x ContainerType
    => [EntityWithId] -> Update x [EntityWithId]
fitIntoContainer ees = do
    let (smallItems, otherItems) = splitItemKind ItemKind_SmallItem ees
    overflow <- go smallItems
    return $ otherItems <> overflow
    where
    go [] = return []
    go (e:es) = do
        cv <- use (self.contentVolume)
        mv <- use (self.containerType.maxVolume)
        if cv >= mv
        then return (e:es)
        else do
            let eid = e^.entityId
            self.content       %= (eid:)
            self.contentVolume += fromMaybe 0 (e^.entity.oracle.volume)
            addAction eid . EntityAction_SelfPassedTo =<< useSelfId
            go es

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

addAction :: EntityId -> EntityAction -> Update x ()
addAction i x = actions %= (:>DirectedEntityAction i x)

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
    rgid = fromIntegral $ hash eid
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

renderSprite :: HasResources c ResourceMap => c -> Resource -> RenderAction
renderSprite ctx r = case lookupResource r $ ctx^.resources of
    Nothing  -> renderShape shape
    Just img -> renderImg img & scale (r^.unitsPerPixel)
    where
    shape = def
        & shapeType   .~ SimpleSquare
        & color       .~ Color.opaque Color.gray

renderAppearance :: HasResources c ResourceMap => c -> Appearance -> RenderAction
renderAppearance ctx = \case
    Appearance_SimpleCircle s c -> renderCircle s c
    Appearance_SimpleSquare s c -> renderSquare s c
    Appearance_Sprite         r -> renderSprite ctx r
    Appearance_Translate    t a -> translate t $ renderAppearance ctx a
    Appearance_Compose as -> renderComposition $ map (renderAppearance ctx) as
    where
    renderCircle = renderS SimpleCircle
    renderSquare = renderS SimpleSquare
    renderS t s c = scale s $ renderShape $ def
        & shapeType .~ t
        & color     .~ c

renderBBox :: BBox Float -> RenderAction
renderBBox bb = renderSimpleBox $ def
    & size .~ (view size $ bboxToRect bb)
    & border.each.color .~ Color.opaque Color.gray
    & border.each.width .~ (1/32)

--------------------------------------------------------------------------------
-- Queries

queryStaticInRange :: RangeBBox -> Update s [EntityWithId]
queryStaticInRange rng =
    EntityIndex.lookupInRange EntityKind_Static rng =<< use (context.entities)

--------------------------------------------------------------------------------
-- Utils

anyMatch
    :: HasProcessOnUpdate x [EntityAction]
    => APrism' EntityAction y
    -> Update x ()
    -> Update x ()
anyMatch p act = do
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

