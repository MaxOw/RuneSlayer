module Types.Entity.Projectile where

import Delude
import Types.Entity.Item (ItemType)
import Types.Entity.Common
import Types.Entity.ZIndex

data Projectile = Projectile
   { field_location     :: Location
   , field_velocity     :: Velocity
   , field_distanceLeft :: Distance
   , field_attackPower  :: AttackPower
   , field_target       :: EntityId
   , field_itemType     :: ItemType
   } deriving (Generic)

instance GetZIndex Projectile Word32 where
    get_zindex _ = toZIndex EntityZIndex_Vertical

