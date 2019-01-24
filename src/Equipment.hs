module Equipment
    -- ( Equipment, EquipmentSlot (..)
    ( module Types.Equipment

    , create
    , lookup
    , insert
    , member
    , emptySlots
    ) where

import Delude
import qualified Data.Map as Map
import qualified Data.Set as Set

import Types.Equipment
import Types.Entity.Common

create :: Set EquipmentSlot -> Equipment
create s = set slots s def

member :: EquipmentSlot -> Equipment -> Bool
member k = Set.member k . view slots

lookup :: EquipmentSlot -> Equipment -> Maybe EntityId
lookup k = Map.lookup k . view content

insert :: EquipmentSlot -> EntityId -> Equipment -> Equipment
insert k v e = over content (if member k e then Map.insert k v else id) e

emptySlots :: Equipment -> Set EquipmentSlot
emptySlots e = Set.difference ss ks
    where
    ss = view slots e
    ks = Map.keysSet $ view content e
