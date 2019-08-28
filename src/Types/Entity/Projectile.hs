module Types.Entity.Projectile where

import Delude
import Types.Entity.Passive
import Types.Entity.Common
import Types.Entity.ZIndex

data Projectile = Projectile
   { field_location     :: Location
   , field_velocity     :: Velocity
   , field_distanceLeft :: Distance
   , field_attackPower  :: AttackPower
   , field_source       :: Maybe EntityId
   , field_target       :: EntityId
   , field_passiveType  :: PassiveType
   } deriving (Generic)

instance GetZIndex Projectile Word32 where
    get_zindex _ = toZIndex EntityZIndex_Vertical

