{-# Language TemplateHaskell #-}
module Types.Equipment where

import Delude
import Types.Entity.Common

data EquipmentSlot
   = EquipmentSlot_Backpack
   -- | EquipmentSlot_Bundle
   | EquipmentSlot_Belt

   | EquipmentSlot_Head
   | EquipmentSlot_Torso
   | EquipmentSlot_Hands
   | EquipmentSlot_Legs
   | EquipmentSlot_Feet
   deriving (Show, Eq, Ord)

data Equipment = Equipment
   { equipment_slots   :: Set EquipmentSlot
   , equipment_content :: Map EquipmentSlot EntityId
   }
makeFieldsCustom ''Equipment

instance Default Equipment where
   def = Equipment
    { equipment_slots   = mempty
    , equipment_content = mempty
    }
