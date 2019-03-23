{-# Language TemplateHaskell #-}
module Types.Entity.Unit where

import Delude
import Types.EntityAction
import Types.Entity.Common
import Types.Entity.ZIndex
import Types.Entity.ItemType (ItemTypeName)
import Types.Entity.Animation
    (Animation, AnimationState, AnimationDesc, EffectState)

--------------------------------------------------------------------------------

newtype UnitTypeName = UnitTypeName { unUnitTypeName :: Text }
    deriving (Default, Eq, Hashable, Generic, ToJSON, FromJSON)
data UnitType = UnitType
   { unitType_name      :: UnitTypeName
   , unitType_corpse    :: Maybe ItemTypeName
   , unitType_animation :: AnimationDesc
   , unitType_maxHealth :: Health
   } deriving (Generic)

data Unit = Unit
   { unit_location        :: Location
   , unit_velocity        :: Velocity
   , unit_animationState  :: AnimationState
   , unit_animation       :: Animation
   , unit_effects         :: [EffectState]
   , unit_health          :: Health
   , unit_unitType        :: UnitType
   , unit_isMarked        :: Bool
   , unit_processOnUpdate :: [EntityAction]
   } deriving (Generic)

instance GetZIndex Unit Word32 where
    get_zindex _ = toZIndex EntityZIndex_Vertical

--------------------------------------------------------------------------------

makeFieldsCustom ''UnitType
instance Default UnitType

instance ToJSON   UnitType where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON UnitType where parseJSON  = genericParseJSON  customOptionsJSON

makeFieldsCustom ''Unit
instance Default Unit
