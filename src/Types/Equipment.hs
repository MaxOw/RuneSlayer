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
   | EquipmentSlot_PrimaryWeapon
   | EquipmentSlot_PrimaryOther
   | EquipmentSlot_SecondaryWeapon
   | EquipmentSlot_SecondaryOther
   deriving (Show, Eq, Ord, Generic)
instance ToJSON EquipmentSlot where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON EquipmentSlot where
    parseJSON = genericParseJSON customOptionsJSON

data Equipment = Equipment
   { field_slots   :: Set EquipmentSlot
   , field_content :: Bimap EquipmentSlot EntityId
   } deriving (Generic)

equipmentRenderOrder :: [EquipmentSlot]
equipmentRenderOrder =
    [ EquipmentSlot_Backpack
    , EquipmentSlot_Feet
    , EquipmentSlot_Legs
    , EquipmentSlot_Torso
    , EquipmentSlot_Belt
    , EquipmentSlot_Head
    , EquipmentSlot_Hands
    , EquipmentSlot_PrimaryWeapon
    , EquipmentSlot_PrimaryOther ]

instance Default Equipment
