{-# Language TemplateHaskell #-}
module Types.Entity.ItemType where

import Delude

import Types.Equipment
import Types.Entity.Common
import Types.Entity.Appearance

--------------------------------------------------------------------------------

data ItemKind
   = ItemKind_Container
   | ItemKind_SmallItem
   | ItemKind_BigItem
   deriving (Eq, Generic)

newtype ItemTypeName = ItemTypeName { unItemTypeName :: Text }
    deriving (Eq, Hashable, Generic, ToJSON, FromJSON)

data ItemType = ItemType
   { itemType_name         :: ItemTypeName
   , itemType_volume       :: Volume
   , itemType_itemKind     :: ItemKind
   , itemType_appearance   :: Appearance
   , itemType_fittingSlots :: Set EquipmentSlot
   } deriving (Generic)

data ContainerType = ContainerType
   { containerType_maxVolume :: Volume
   , containerType_itemType  :: ItemType
   } deriving (Generic)

--------------------------------------------------------------------------------

instance Default ItemKind where def = ItemKind_SmallItem
instance ToJSON ItemKind where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON ItemKind where
    parseJSON = genericParseJSON customOptionsJSON

makeWrapped ''ItemTypeName
instance Default ItemTypeName

makeFieldsCustom ''ItemType
instance Default ItemType
instance ToJSON ItemType where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON ItemType where
    parseJSON = genericParseJSON customOptionsJSON

makeFieldsCustom ''ContainerType
instance Default ContainerType

