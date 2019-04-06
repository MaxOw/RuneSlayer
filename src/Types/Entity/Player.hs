module Types.Entity.Player where

import Delude
import qualified Data.Set as Set

import Types.EntityAction
import Types.Entity.Common
import Types.Entity.ZIndex
import Types.Entity.Animation
import Types.Equipment
import Types.Skills.Runes (RunicLevel)
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
instance Default Player

instance GetZIndex Player Word32 where
    get_zindex _ = toZIndex EntityZIndex_Vertical



