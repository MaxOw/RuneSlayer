{-# Language TemplateHaskell #-}
module Types.Entity.Player where

import Delude
import qualified Data.Set as Set

import Types.EntityAction
import Types.Entity.Common
import Types.Entity.ZIndex
import Types.Entity.Animation (Animation)
import Types.Equipment
import Types.Skills.Runes (RunicLevel)
import qualified Equipment

--------------------------------------------------------------------------------

data Player = Player
   { player_location        :: Location
   , player_velocity        :: Velocity
   , player_maxSpeed        :: Speed
   , player_equipment       :: Equipment
   , player_debugFlags      :: EntityDebugFlags
   , player_processOnUpdate :: [EntityAction]
   , player_collisionShape  :: Maybe CollisionShape
   , player_animation       :: Animation
   , player_runicLevel      :: RunicLevel
   , player_target          :: Maybe EntityId
   } deriving (Generic)
makeFieldsCustom ''Player

instance Default Player where
   def = Player
    { player_location        = def
    , player_velocity        = def
    , player_maxSpeed        = baseWalkingSpeed
    -- , player_maxSpeed        = baseRunningSpeed
    , player_equipment       = Equipment.create playerSlots
    , player_debugFlags      = def
    , player_processOnUpdate = def
    , player_collisionShape  = def
    , player_animation       = def
    , player_runicLevel      = def
    , player_target          = Nothing
    }

instance GetZIndex Player Word32 where
    get_zindex _ = toZIndex EntityZIndex_Vertical

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

