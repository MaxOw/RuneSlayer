{-# Language TemplateHaskell #-}
module Types.Entity.Unit where

import Delude
import Types.EntityAction
import Types.Entity.Common
import Types.Entity.ZIndex
import Types.Entity.Animation (Animation, EffectState)
import Resource (Resource)

--------------------------------------------------------------------------------

data UnitType = UnitType
   { unitType_animation :: Animation -- This should be AnimationDesc in future
   , unitType_sprite    :: Resource
   , unitType_maxHealth :: Health
   } deriving (Generic)
makeFieldsCustom ''UnitType
instance Default UnitType

data Unit = Unit
   { unit_location        :: Location
   , unit_velocity        :: Velocity
   , unit_animation       :: Animation
   , unit_effects         :: [EffectState]
   , unit_health          :: Health
   , unit_unitType        :: UnitType
   , unit_isMarked        :: Bool
   , unit_processOnUpdate :: [EntityAction]
   } deriving (Generic)
makeFieldsCustom ''Unit
instance Default Unit

instance GetZIndex Unit Word32 where
    get_zindex _ = toZIndex EntityZIndex_Vertical

