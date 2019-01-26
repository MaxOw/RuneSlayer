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
   , equipment_content :: Bimap EquipmentSlot EntityId
   } deriving (Generic)
makeFieldsCustom ''Equipment

instance Default Equipment
