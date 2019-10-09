module Equipment
    -- ( Equipment, EquipmentSlot (..)
    ( module Types.Equipment

    , create
    , hasSlot, hasId
    , lookupSlot
    , insert, alter
    , deleteAll
    , deleteId
    , emptySlots
    , selectSlots, excludeSlots
    , contentList, slotsList
    ) where

import Delude
import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import qualified Data.Set as Set

import Types.Equipment
import Types.Entity.Common

create :: Set EquipmentSlot -> Equipment
create s = set slots s def

hasSlot :: EquipmentSlot -> Equipment -> Bool
hasSlot k = Set.member k . view slots

hasId :: EntityId -> Equipment -> Bool
hasId v = Bimap.memberR v . view content

lookupSlot :: EquipmentSlot -> Equipment -> Maybe EntityId
lookupSlot k = Bimap.lookup k . view content

insert :: EquipmentSlot -> EntityId -> Equipment -> Equipment
insert k v e = if hasSlot k e
    then e & content %~ Bimap.insert k v
    else e

alter :: (Maybe EntityId -> Maybe EntityId) -> EquipmentSlot -> Equipment -> Equipment
alter f k e = if hasSlot k e
    then e & content %~ bimapAlter f k
    else e

bimapAlter :: (Ord k, Ord a) => (Maybe a -> Maybe a) -> k -> Bimap k a -> Bimap k a
bimapAlter f k b = case f $ Bimap.lookup k b of
    Nothing -> Bimap.delete k b
    Just nv -> Bimap.insert k nv b

deleteAll :: Equipment -> Equipment
deleteAll = set content Bimap.empty

deleteId :: EntityId -> Equipment -> Equipment
deleteId v = over content (Bimap.deleteR v)

emptySlots :: Equipment -> Set EquipmentSlot
emptySlots e = Set.difference ss ks
    where
    ss = view slots e
    ks = Map.keysSet . Bimap.toMap $ view content e

contentList :: Equipment -> [EntityId]
contentList = Bimap.elems . view content

excludeSlots :: Equipment -> [EquipmentSlot] -> [EntityId]
excludeSlots eq r
    = Map.elems $ flip Map.withoutKeys (Set.fromList r)
    $ Bimap.toMap $ view content eq

selectSlots :: Equipment -> [EquipmentSlot] -> [EntityId]
selectSlots eq r
    = Map.elems $ flip Map.restrictKeys (Set.fromList r)
    $ Bimap.toMap $ view content eq

slotsList :: Equipment -> [(EquipmentSlot, EntityId)]
slotsList = Bimap.assocs . view content

