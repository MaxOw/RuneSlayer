{-# Language TemplateHaskell #-}
module Types.Entity.Player where

import Delude
import qualified Data.Set as Set

import Types.EntityAction
import Types.Entity.Common
import Types.Entity.ZIndex
import Types.Equipment
import qualified Equipment

--------------------------------------------------------------------------------

data Player = Player
   { player_location        :: Location
   , player_velocity        :: Velocity
   , player_maxSpeed        :: Speed
   , player_equipment       :: Equipment
   , player_debugFlags      :: EntityDebugFlags
   , player_processOnUpdate :: [EntityAction]
   } deriving (Generic)
makeFieldsCustom ''Player

instance Default Player where
   def = Player
    { player_location        = def
    , player_velocity        = def
    , player_maxSpeed        = baseRunningSpeed
    , player_equipment       = Equipment.create playerSlots
    , player_debugFlags      = def
    , player_processOnUpdate = def
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

