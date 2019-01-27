{-# Language TemplateHaskell #-}
module Types.Entity.ItemType where

import Delude

import Types.Equipment
import Types.Entity.Common
import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color

data ItemKind
   = ItemKind_Container
   | ItemKind_SmallItem
   | ItemKind_BigItem
   deriving (Eq)
instance Default ItemKind where def = ItemKind_SmallItem

data Appearance
   = Appearance_SimpleCircle Double AlphaColor
   | Appearance_SimpleSquare Double AlphaColor
instance Default Appearance where
    def = Appearance_SimpleCircle 1 $ Color.opaque Color.red

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

