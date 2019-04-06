module Types.Entity.Unit where

import Delude
import Types.EntityAction
import Types.Entity.Timer
import Types.Entity.Reactivity
import Types.Entity.Common
import Types.Entity.ZIndex
import Types.Entity.ItemType (ItemTypeName)
import Types.Entity.Animation
    (Animation, AnimationState, AnimationDesc, EffectState)

--------------------------------------------------------------------------------

newtype UnitTypeName = UnitTypeName { unUnitTypeName :: Text }
    deriving (Default, Eq, Hashable, Generic, ToJSON, FromJSON)
data UnitType = UnitType
   { field_name               :: UnitTypeName
   , field_corpse             :: Maybe ItemTypeName
   , field_animation          :: AnimationDesc
   , field_animateWhenStopped :: Bool
   , field_maxHealth          :: Health
   , field_maxSpeed           :: Speed
   , field_attackRange        :: Distance
   , field_attackPower        :: AttackPower
   , field_attackSpeed        :: Duration
   , field_aggroRange         :: Distance
   , field_pursueRange        :: Distance
   , field_reactivity         :: Map ReactivCategory ReactivValue
   , field_hostileTowards     :: Set ReactivCategory
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
   , field_target          :: Maybe EntityId
   , field_timer           :: Timer
   } deriving (Generic)
instance HasAnimateWhenStopped Unit Bool where
    animateWhenStopped = unitType.ff#animateWhenStopped
instance HasMaxSpeed Unit Speed where
    maxSpeed = unitType.ff#maxSpeed

instance GetZIndex Unit Word32 where
    get_zindex _ = toZIndex EntityZIndex_Vertical

--------------------------------------------------------------------------------

instance Default UnitType

instance ToJSON   UnitType where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON UnitType where parseJSON  = genericParseJSON  customOptionsJSON

instance Default Unit
