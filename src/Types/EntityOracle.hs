module Types.EntityOracle where

import Delude
import Types.Entity.Reactivity
import Types.Equipment
import Types.Entity.Common
import Types.Entity.Item
import Types.Entity.ZIndex
import Types.Entity.Animation

data EntityOracle = EntityOracle
   { field_name           :: Maybe Text
   , field_location       :: Maybe Location
   , field_equipment      :: Maybe Equipment
   , field_itemKind       :: Maybe ItemKind
   , field_content        :: Maybe [EntityId]
   , field_volume         :: Maybe Volume
   , field_maxVolume      :: Maybe Volume
   , field_fittingSlots   :: Set EquipmentSlot
   , field_zindex         :: Maybe EntityZIndex
   , field_collisionShape :: Maybe CollisionShape
   , field_reactivity     :: Map ReactivCategory ReactivValue
   , field_itemAnimation  :: Maybe AnimationName
   } deriving (Generic)


instance Default EntityOracle
