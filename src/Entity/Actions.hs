module Entity.Actions
    -- ActOn Actions
    ( setMoveVector
    , toggleDebugFlag
    , handleOnUpdate

    -- Update Actions
    , pureIntegrateLocation, integrateLocation
    , pureAddItems, addItems
    , pureDropAllItems, dropAllItems
    , purePickUpInformOwner, pickUpInformOwner
    , dropItem

    -- Render Actions
    , maybeLocate

    -- Utils
    , anyMatch
    , ifJustLocation
    ) where

import Delude
import qualified Data.Set as Set
import Random.Utils

import Types.Entity
import Entity.Utils
import EntityIndex (lookupEntityById)
import qualified Equipment
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

pureDropAllItems
    :: HasProcessOnUpdate x [EntityAction]
    => HasEquipment       x Equipment
    => HasLocation        x Location
    => x -> EntityContext -> (x, [DirectedEntityAction])
pureDropAllItems p ctx = if any isDropAllItems $ p^.processOnUpdate
    then (pp, is)
    else (p,  [])
    where
    fct = ctx^.frameCount
    pp = p & equipment %~ Equipment.deleteAll
    is = map (makeDropItem fct $ p^.location) $ p^.equipment.to contentList

    isDropAllItems EntityAction_DropAllItems = True
    isDropAllItems _                         = False

dropAllItems
    :: HasProcessOnUpdate x [EntityAction]
    => HasEquipment       x Equipment
    => HasLocation        x Location
    => Update             x ()
dropAllItems = do
    (newSelf, newActions) <- pureDropAllItems <$> use self <*> use context
    self .= newSelf
    actions <>= fromList newActions

--------------------------------------------------------------------------------

purePickUpInformOwner
    :: HasOwner x (Maybe EntityId)
    => x -> EntityContext -> [DirectedEntityAction]
purePickUpInformOwner x ctx = case x^.owner of
    Just ownerId -> [DirectedEntityAction ownerId act]
    _            -> []
    where
    act = EntityAction_AddToInventory $ ctx^.selfId

pickUpInformOwner
    :: HasOwner x (Maybe EntityId)
    => Update x ()
pickUpInformOwner = do
    newActions <- purePickUpInformOwner <$> use self <*> use context
    actions <>= fromList newActions

--------------------------------------------------------------------------------

pureAddItems
    :: HasProcessOnUpdate x [EntityAction]
    => HasEquipment       x Equipment
    => HasLocation        x Location
    => EntityContext -> x -> (x, [DirectedEntityAction])
pureAddItems ctx p = (pp, ds)
    where
    ds = map (makeDropItem (ctx^.frameCount) (p^.location) . view entityId) rs
    (pp, rs) = equipItems (getItemsToAdd ctx p) p
    -- for each entityId to add
    -- lookup entity for that id
    --
    -- if there is no bag equiped find and equip
    -- try to equip other equipable items
    -- collect small items
    -- if

addItems
    :: HasProcessOnUpdate x [EntityAction]
    => HasEquipment       x Equipment
    => HasLocation        x Location
    => Update x ()
addItems = do
    (newSelf, newActions) <- pureAddItems <$> use context <*> use self
    self .= newSelf
    actions <>= fromList newActions

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
    return ()

--------------------------------------------------------------------------------

addAction :: DirectedEntityAction -> Update x ()
addAction x = actions %= (:>x)

dropItemAction
    :: HasLocation x Location
    => EntityId -> Update x ()
dropItemAction eid = do
    loc <- use $ self.location
    fct <- use $ context.frameCount
    addAction $ makeDropItem fct loc eid

makeDropItem :: Word32 -> Location -> EntityId -> DirectedEntityAction
makeDropItem rnd loc eid = DirectedEntityAction eid (EntityAction_SelfDropAt dloc)
    where
    rgid = fromIntegral $ unEntityId eid
    dloc = over _Wrapped (\v -> v + genDropOffset [rnd + rgid]) loc

equipItems
    :: HasEquipment x Equipment
    => [EntityWithId] -> x -> (x, [EntityWithId])
equipItems eis p = go p (emptySlots p) [] eis
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
    g (EntityAction_AddToInventory i) = Just i
    g _                               = Nothing

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

