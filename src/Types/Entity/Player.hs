module Types.Entity.Player where

import Delude
import qualified Data.Set as Set

import Types.EntityAction
import Types.Entity.Common
import Types.Entity.ZIndex
import Types.Entity.Animation (AnimationState, Animation, EffectState)
import Types.Equipment
import Types.Skills.Runes (RunicLevel)
import Types.Entity.Reactivity
import qualified Equipment

--------------------------------------------------------------------------------

data PlayerInit = PlayerInit
   { field_body        :: [Text] -- BodyDesc
   , field_reactivity  :: Map ReactivCategory ReactivValue
   , field_attackRange :: Distance
   } deriving (Generic)

data Player = Player
   { field_location           :: Location
   , field_velocity           :: Velocity
   , field_maxSpeed           :: Speed
   , field_equipment          :: Equipment
   , field_debugFlags         :: EntityDebugFlags
   , field_processOnUpdate    :: [EntityAction]
   , field_collisionShape     :: Maybe CollisionShape
   , field_animationState     :: AnimationState
   , field_animateWhenStopped :: Bool
   , field_bodyAnimation      :: Animation
   , field_effects            :: [EffectState]
   , field_runicLevel         :: RunicLevel
   , field_target             :: Maybe EntityId
   , field_reactivity         :: Map ReactivCategory ReactivValue
   , field_attackRange        :: Distance
   } deriving (Generic)
instance HasAnimateWhenStopped Player Bool
instance HasMaxSpeed Player Speed

playerSlots :: Set EquipmentSlot
playerSlots = Set.fromList
    [ EquipmentSlot_Backpack
    -- , EquipmentSlot_Bundle
    , EquipmentSlot_Belt
    , EquipmentSlot_Head
    , EquipmentSlot_Torso
    , EquipmentSlot_Hands
    , EquipmentSlot_Legs
    , EquipmentSlot_Feet ]

--------------------------------------------------------------------------------

instance ToJSON PlayerInit where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON PlayerInit where
    parseJSON = genericParseJSON customOptionsJSON

instance Default PlayerInit
instance Default Player where
   def = Player
    { field_location           = def
    , field_velocity           = def
    , field_maxSpeed           = baseWalkingSpeed
    -- , field_maxSpeed        = baseRunningSpeed
    , field_equipment          = Equipment.create playerSlots
    , field_debugFlags         = def
    , field_processOnUpdate    = def
    , field_collisionShape     = def
    , field_animationState     = def
    , field_animateWhenStopped = False
    , field_bodyAnimation      = def
    , field_effects            = []
    , field_runicLevel         = def
    , field_target             = Nothing
    , field_reactivity         = def
    , field_attackRange        = disM 2
    }

instance GetZIndex Player Word32 where
    get_zindex _ = toZIndex EntityZIndex_Vertical



