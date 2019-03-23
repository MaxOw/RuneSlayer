{-# Language TemplateHaskell #-}
module Types.Entity.Player where

import Delude
import qualified Data.Set as Set

import Types.EntityAction
import Types.Entity.Common
import Types.Entity.ZIndex
import Types.Entity.Animation (AnimationState, Animation)
import Types.Equipment
import Types.Skills.Runes (RunicLevel)
import qualified Equipment

--------------------------------------------------------------------------------

data PlayerInit = PlayerInit
   { playerInit_body :: [Text] -- BodyDesc
   } deriving (Generic)

data Player = Player
   { player_location        :: Location
   , player_velocity        :: Velocity
   , player_maxSpeed        :: Speed
   , player_equipment       :: Equipment
   , player_debugFlags      :: EntityDebugFlags
   , player_processOnUpdate :: [EntityAction]
   , player_collisionShape  :: Maybe CollisionShape
   , player_animationState  :: AnimationState
   , player_bodyAnimation   :: Animation
   , player_runicLevel      :: RunicLevel
   , player_target          :: Maybe EntityId
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
    , EquipmentSlot_Feet ]

--------------------------------------------------------------------------------

instance ToJSON PlayerInit where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON PlayerInit where
    parseJSON = genericParseJSON customOptionsJSON

instance Default PlayerInit
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
    , player_animationState  = def
    , player_bodyAnimation   = def
    , player_runicLevel      = def
    , player_target          = Nothing
    }

instance GetZIndex Player Word32 where
    get_zindex _ = toZIndex EntityZIndex_Vertical

makeFieldsCustom ''PlayerInit
makeFieldsCustom ''Player
