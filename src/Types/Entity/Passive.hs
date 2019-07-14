{-# Language TemplateHaskell #-}
module Types.Entity.Passive where

import Delude

import Types.Equipment
import Types.EntityAction
import Types.Entity.Animation
import Types.Entity.Common
import Types.Entity.Appearance
-- import Types.Entity.ZIndex
import Types.Entity.Animation (Direction)

--------------------------------------------------------------------------------

data WeaponKind
   = WeaponKind_Slashing
   | WeaponKind_Thrusting
   | WeaponKind_Projecting
   deriving (Eq, Generic)

data PassiveKind
   = PassiveKind_Container
   | PassiveKind_SmallItem
   | PassiveKind_BigItem
   | PassiveKind_Projectile
   | PassiveKind_Arrow
   | PassiveKind_Item
   deriving (Eq, Ord, Generic)

newtype PassiveTypeName = PassiveTypeName { unPassiveTypeName :: Text }
    deriving (Show, Eq, Hashable, Generic, ToJSON, FromJSON)

data UseActionEffect
   = UseActionEffect_TransformInto PassiveTypeName
   | UseActionEffect_InspectContent
   | UseActionEffect_DeleteSelf
   | UseActionEffect_Heal Health
   deriving (Generic, Show)

data PassiveType = PassiveType
   { field_name          :: PassiveTypeName
   , field_volume        :: Volume
   , field_passiveKind   :: Set PassiveKind
   , field_weaponKind    :: Maybe WeaponKind
   , field_stats         :: Stats
   , field_appearance    :: Appearance
   -- Animation when equipped, TODO: rename it as such.
   , field_animation     :: Maybe AnimationName
   , field_behindBody    :: Maybe Bool
   , field_fittingSlots  :: Set EquipmentSlot
   , field_containerType :: Maybe ContainerType
   , field_useActions    :: Map UseActionName [UseActionEffect]
   , field_zindex        :: Word32
   } deriving (Generic)

data ContainerType = ContainerType
   { field_maxVolume  :: Volume
   , field_allowKinds :: Set PassiveKind
   , field_showCount  :: Bool
   } deriving (Generic)

--------------------------------------------------------------------------------

data Passive = Passive
   { field_location        :: Maybe Location
   , field_owner           :: Maybe EntityId
   , field_passiveType     :: PassiveType
   , field_processOnUpdate :: [EntityAction]
   , field_content         :: [EntityId]
   , field_contentVolume   :: Volume
   , field_direction       :: Maybe Direction
   , field_animationState  :: AnimationState
   , field_animation       :: Animation
   , field_centerOffset    :: V2D -- This is such a duct-tape...
   } deriving (Generic)

--------------------------------------------------------------------------------

instance Default Passive
instance GetZIndex Passive Word32 where get_zindex x = x^.passiveType.zindex

instance ToJSON PassiveKind where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON PassiveKind where parseJSON = genericParseJSON customOptionsJSON

instance ToJSON WeaponKind where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON WeaponKind where parseJSON = genericParseJSON customOptionsJSON

makeWrapped ''PassiveTypeName
instance Default PassiveTypeName

instance ToJSON UseActionEffect where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON UseActionEffect where parseJSON = genericParseJSON customOptionsJSON

instance Default PassiveType
instance ToJSON PassiveType where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON PassiveType where parseJSON = genericParseJSON customOptionsJSON

instance Default ContainerType
instance ToJSON ContainerType where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON ContainerType where parseJSON = genericParseJSON customOptionsJSON

--------------------------------------------------------------------------------

passiveKind :: Lens' PassiveType (Set PassiveKind)
passiveKind = ff#passiveKind

allowKinds :: Lens' ContainerType (Set PassiveKind)
allowKinds = ff#allowKinds

useActions :: Lens' PassiveType (Map UseActionName [UseActionEffect])
useActions = ff#useActions

behindBody :: Lens' PassiveType (Maybe Bool)
behindBody = ff#behindBody

stats :: Lens' PassiveType Stats
stats = ff#stats

showCount :: Lens' ContainerType Bool
showCount = ff#showCount

centerOffset :: Lens' Passive V2D
centerOffset = ff#centerOffset

