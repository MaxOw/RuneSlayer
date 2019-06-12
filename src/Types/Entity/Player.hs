module Types.Entity.Player where

import Delude
import qualified Data.Set as Set

import Types.EntityAction
import Types.Entity.Common
import Types.Entity.ZIndex
import Types.Entity.Animation
import Types.Entity.Timer
import Types.Equipment
import Types.Skills.Runes (RunicLevel, RunicSlots, RuneName)
import Types.Entity.Reactivity

--------------------------------------------------------------------------------

data UpdateOnce
   = UpdateOnce_Equipment
   deriving (Eq, Ord)

data PlayerInit = PlayerInit
   { field_body        :: [AnimationName] -- BodyDesc
   , field_reactivity  :: Map ReactivCategory ReactivValue
   , field_attackRange :: Distance
   , field_maxSpeed    :: Speed
   } deriving (Generic)
instance HasMaxSpeed PlayerInit Speed

data DelayedActionType
   = DelayedActionType_Attack EntityId AttackPower
   | DelayedActionType_FireProjectile Location V2D EntityId EntityId AttackPower

data DelayedAction = DelayedAction
   { field_timeLeft :: Duration
   , field_action   :: DelayedActionType
   } deriving (Generic)

data Player = Player
   { field_location           :: Location
   , field_velocity           :: Velocity
   , field_maxSpeed           :: Speed
   , field_equipment          :: Equipment
   , field_debugFlags         :: EntityDebugFlags
   , field_processOnUpdate    :: [EntityAction]
   , field_updateOnce         :: Set UpdateOnce
   , field_collisionShape     :: Maybe CollisionShape
   , field_animationState     :: AnimationState
   , field_animateWhenStopped :: Bool
   , field_bodyAnimation      :: Animation
   , field_equipmentAnimation :: Animation
   , field_runicLevel         :: RunicLevel
   , field_offensiveSlots     :: RunicSlots
   , field_defensiveSlots     :: RunicSlots
   , field_target             :: Maybe EntityId
   , field_reactivity         :: Map ReactivCategory ReactivValue
   , field_attackRange        :: Distance
   , field_status             :: Set EntityStatus
   , field_timer              :: Timer
   , field_attackMode         :: AttackMode
   , field_selectedRune       :: Maybe RuneName
   , field_delayedActions     :: [DelayedAction]
   } deriving (Generic)
instance HasAnimateWhenStopped Player Bool
instance HasMaxSpeed Player Speed

-- Player fields needed when displaying UI
data PlayerStatus = PlayerStatus
   { field_runicLevel         :: RunicLevel
   , field_offensiveSlots     :: RunicSlots
   , field_defensiveSlots     :: RunicSlots
   , field_selectedRune       :: Maybe RuneName
   , field_status             :: Set EntityStatus
   , field_attackMode         :: AttackMode
   } deriving (Generic)

playerSlots :: Set EquipmentSlot
playerSlots = Set.fromList
    [ EquipmentSlot_Backpack
    -- , EquipmentSlot_Bundle
    , EquipmentSlot_Belt
    , EquipmentSlot_Head
    , EquipmentSlot_Torso
    , EquipmentSlot_Hands
    , EquipmentSlot_Legs
    , EquipmentSlot_Feet
    , EquipmentSlot_PrimaryWeapon
    , EquipmentSlot_PrimaryOther  ]

--------------------------------------------------------------------------------

instance ToJSON PlayerInit where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON PlayerInit where
    parseJSON = genericParseJSON customOptionsJSON

instance Default PlayerInit
instance Default Player

instance GetZIndex Player Word32 where
    get_zindex _ = toZIndex EntityZIndex_Vertical



