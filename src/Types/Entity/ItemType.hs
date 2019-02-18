{-# Language TemplateHaskell #-}
module Types.Entity.ItemType where

import Delude

import Types.Equipment
import Types.Entity.Common
import Types.Entity.Appearance

data ItemKind
   = ItemKind_Container
   | ItemKind_SmallItem
   | ItemKind_BigItem
   deriving (Eq)
instance Default ItemKind where def = ItemKind_SmallItem

data ItemType = ItemType
   { itemType_name         :: Text
   , itemType_volume       :: Volume
   , itemType_itemKind     :: ItemKind
   , itemType_appearance   :: Appearance
   , itemType_fittingSlots :: Set EquipmentSlot
   } deriving (Generic)
makeFieldsCustom ''ItemType
instance Default ItemType

data ContainerType = ContainerType
   { containerType_maxVolume :: Volume
   , containerType_itemType  :: ItemType
   } deriving (Generic)
makeFieldsCustom ''ContainerType
instance Default ContainerType

--------------------------------------------------------------------------------

