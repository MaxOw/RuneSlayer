module Entity.Actions
    -- ActOn Actions
    ( setMoveVector
    , toggleDebugFlag
    , handleOnUpdate

    -- Update Actions
    , pureIntegrateLocation, integrateLocation
    , addItems
    , dropAllItems
    , purePickUpInformOwner, pickUpInformOwner
    , dropItem
    , containerAddItems

    -- Render Actions
    , maybeLocate

    -- Utils
    , anyMatch
    , ifJustLocation
    ) where

import Delude
import qualified Data.Set as Set
import qualified Data.List as List
import Random.Utils

import Types.Entity
import Types.Entity.ItemType
import Entity.Utils
import EntityIndex (lookupEntityById)
import qualified Equipment
import Types.Equipment
import Equipment (Equipment, contentList)

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

toggleDebugFlag :: HasDebugFlags x EntityDebugFlags => DebugFlag -> x -> x
toggleDebugFlag = \case
    DebugFlag_DrawPickupRange -> flipFlag drawPickupRange
    where
    flipFlag f = over (debugFlags.f) not

handleOnUpdate :: HasProcessOnUpdate s [a] => a -> s -> s
handleOnUpdate a = over processOnUpdate (a:)

--------------------------------------------------------------------------------
-- Update Actions

pureIntegrateLocation
    :: HasVelocity s Velocity
    => HasLocation s Location
 -- => Delta Time -> s -> s
    => s -> s
pureIntegrateLocation s = set location newLoc s
    where
    newLoc = upd defaultDelta (s^.velocity) (s^.location)
    upd (Time t) (Velocity v) (Location l) = Location $ l ^+^ (v^*t)

integrateLocation
    :: HasVelocity s Velocity
    => HasLocation s Location
    => Update s ()
integrateLocation = liftUpdate pureIntegrateLocation

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
    os <- equipItems =<< getItemsToAdd <$> use context <*> use self
    let (sit, oit) = splitItemKind ItemKind_SmallItem os
    whenJustM equippedBackpack $ \ct -> do
        -- sendItemsToContainer ct sit
        mapM_ (addAction ct . EntityAction_AddItem . view entityId) sit
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
    os <- fitIntoContainer =<< getItemsToAdd <$> use context <*> use self
    mapM_ containerDropItem os

containerDropItem
    :: HasLocation x (Maybe Location)
    => HasOwner    x (Maybe EntityId)
    => EntityWithId -> Update x ()
containerDropItem e = use (self.location) >>= \case
    Nothing -> use (self.owner) >>= \mo -> whenJust mo $ \o ->
        addAction o $ EntityAction_DropItem eid
    Just lc -> use (context.frameCount) >>= \fct ->
        addAction eid $ makeDropItem fct lc eid
    where
    eid = e^.entityId

fitIntoContainer
    :: HasContent       x [EntityId]
    => HasContentVolume x Volume
    => HasContainerType x ContainerType
    => [EntityWithId] -> Update x [EntityWithId]
fitIntoContainer es = do
    let (smallItems, otherItems) = splitItemKind ItemKind_SmallItem es
    overflow <- go $ trace ("Here" :: Text) smallItems
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
    => EntityContext -> x -> [EntityWithId]
getItemsToAdd ctx = mapMaybe (f <=< g) . view processOnUpdate
    where
    f i = EntityWithId i <$> lookupEntityById i (ctx^.entities)
    g (EntityAction_AddItem        i) = Just i
    g _                               = Nothing

--------------------------------------------------------------------------------

dropItem
    :: HasEquipment       x Equipment
    => HasLocation        x Location
    => EntityId -> Update x ()
dropItem i = do
    eq <- use $ self.equipment
    when (Equipment.hasId i eq) $ do
        self.equipment %= Equipment.deleteId i
    dropItemAction i

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
    rgid = fromIntegral $ unEntityId eid
    dloc = over _Wrapped (\v -> v + genDropOffset [rnd + rgid]) loc

--------------------------------------------------------------------------------
-- Render Actions

maybeLocate
    :: HasLocation x (Maybe Location)
    => Transformable2D t
    => x -> (t -> t)
maybeLocate x = fromMaybe id $ x^?location.traverse._Wrapped.to translate


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

