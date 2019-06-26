{-# Language TemplateHaskell #-}
module Types.Entity.Item where

import Delude

import Types.Equipment
import Types.EntityAction
import Types.Entity.Animation
import Types.Entity.Common
import Types.Entity.Appearance
import Types.Entity.ZIndex
import Types.Entity.Animation (Direction)

--------------------------------------------------------------------------------

data WeaponKind
   = WeaponKind_Slashing
   | WeaponKind_Thrusting
   | WeaponKind_Projecting
   deriving (Eq, Generic)

data ItemKind
   = ItemKind_Container
   | ItemKind_SmallItem
   | ItemKind_BigItem
   | ItemKind_Projectile
   | ItemKind_Arrow
   deriving (Eq, Ord, Generic)

data ItemUseEffect
   = ItemUseEffect_TransformInto ItemTypeName
   | ItemUseEffect_Heal          Health
   deriving (Generic)

newtype ItemTypeName = ItemTypeName { unItemTypeName :: Text }
    deriving (Eq, Hashable, Generic, ToJSON, FromJSON)

data ItemType = ItemType
   { field_name          :: ItemTypeName
   , field_volume        :: Volume
   , field_itemKind      :: Set ItemKind
   , field_weaponKind    :: Maybe WeaponKind
   , field_stats         :: Stats
   , field_appearance    :: Appearance
   -- Animation when equipped, TODO: rename it as such.
   , field_animation     :: Maybe AnimationName
   , field_fittingSlots  :: Set EquipmentSlot
   , field_containerType :: Maybe ContainerType
   , field_useEffects    :: [ItemUseEffect]
   } deriving (Generic)

data ContainerType = ContainerType
   { field_maxVolume  :: Volume
   , field_allowKinds :: Set ItemKind
   , field_showCount  :: Bool
   } deriving (Generic)

--------------------------------------------------------------------------------

data Item = Item
   { field_location        :: Maybe Location
   , field_owner           :: Maybe EntityId
   , field_itemType        :: ItemType
   , field_processOnUpdate :: [EntityAction]
   , field_content         :: [EntityId]
   , field_contentVolume   :: Volume
   , field_direction       :: Maybe Direction
   , field_animationState  :: AnimationState
   , field_animation       :: Animation
   , field_centerOffset    :: V2D -- This is such a ducktape...
   } deriving (Generic)

--------------------------------------------------------------------------------

instance Default Item
instance GetZIndex Item Word32 where get_zindex _ = toZIndex EntityZIndex_Item

instance Default ItemKind where def = ItemKind_SmallItem
instance ToJSON ItemKind where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON ItemKind where parseJSON = genericParseJSON customOptionsJSON

instance ToJSON WeaponKind where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON WeaponKind where parseJSON = genericParseJSON customOptionsJSON

makeWrapped ''ItemTypeName
instance Default ItemTypeName

instance ToJSON ItemUseEffect where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON ItemUseEffect where parseJSON = genericParseJSON customOptionsJSON

instance Default ItemType
instance ToJSON ItemType where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON ItemType where parseJSON = genericParseJSON customOptionsJSON

instance Default ContainerType
instance ToJSON ContainerType where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON ContainerType where parseJSON = genericParseJSON customOptionsJSON
