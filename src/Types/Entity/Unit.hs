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
   { field_name      :: UnitTypeName
   , field_corpse    :: Maybe ItemTypeName
   , field_animation :: AnimationDesc
   , field_maxHealth :: Health
   } deriving (Generic)

data Unit = Unit
   { field_location        :: Location
   , field_velocity        :: Velocity
   , field_animationState  :: AnimationState
   , field_animation       :: Animation
   , field_effects         :: [EffectState]
   , field_health          :: Health
   , field_unitType        :: UnitType
   , field_isMarked        :: Bool
   , field_processOnUpdate :: [EntityAction]
   } deriving (Generic)

instance GetZIndex Unit Word32 where
    get_zindex _ = toZIndex EntityZIndex_Vertical

--------------------------------------------------------------------------------


instance Default UnitType

instance ToJSON   UnitType where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON UnitType where parseJSON  = genericParseJSON  customOptionsJSON


instance Default Unit
