module Types.EntityOracle where

import Delude
import Types.Equipment
import Types.Entity.Common
import Types.Entity.ItemType
import Types.Entity.ZIndex

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
   } deriving (Generic)


instance Default EntityOracle
