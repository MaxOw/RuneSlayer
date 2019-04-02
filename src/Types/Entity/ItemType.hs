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
   { field_name          :: ItemTypeName
   , field_volume        :: Volume
   , field_itemKind      :: ItemKind
   , field_appearance    :: Appearance
   , field_fittingSlots  :: Set EquipmentSlot
   , field_containerType :: Maybe ContainerType
   } deriving (Generic)

data ContainerType = ContainerType
   { field_maxVolume :: Volume
   } deriving (Generic)

--------------------------------------------------------------------------------

instance Default ItemKind where def = ItemKind_SmallItem
instance ToJSON ItemKind where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON ItemKind where parseJSON = genericParseJSON customOptionsJSON

makeWrapped ''ItemTypeName
instance Default ItemTypeName


instance Default ItemType
instance ToJSON ItemType where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON ItemType where parseJSON = genericParseJSON customOptionsJSON


instance Default ContainerType
instance ToJSON ContainerType where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON ContainerType where parseJSON = genericParseJSON customOptionsJSON
