module Types.Entity.Agent where

import Delude

import Types.EntityAction
import Types.Entity.Common
import Types.Entity.ZIndex
import Types.Entity.Animation
import Types.Entity.Timer
import Types.Entity.Passive
import Types.Entity.Reactivity
import Types.Entity.Script
import Types.Equipment
import Types.Skills.Runes (RunicLevel, RunicPoints, RuneName)

--------------------------------------------------------------------------------

data UpdateOnce
   = UpdateOnce_Equipment
   deriving (Eq, Ord)

-- Record for params describing artificial actors (NPC's & Enemy units)
data UnitType = UnitType
   { field_attackSpeed        :: Duration
   , field_pursueRange        :: Distance
   } deriving (Generic)

newtype AgentTypeName = AgentTypeName { unAgentTypeName :: Text }
    deriving (Default, Eq, Hashable, Generic, ToJSON, FromJSON)

data AgentKind
   = AgentKind_Player
   | AgentKind_Enemy
   | AgentKind_NPC
   deriving (Eq, Generic)

instance Default AgentKind where def = AgentKind_Enemy

data AgentType = AgentType
   { field_name               :: AgentTypeName
   , field_corpse             :: Maybe PassiveTypeName
   , field_reactivity         :: Map ReactivCategory ReactivValue
   , field_autoTargetRange    :: Distance
   , field_hostileTowards     :: Set ReactivCategory

   , field_bodyAnimation      :: [AnimationName]
   , field_animateWhenStopped :: Bool
   , field_renderOffset       :: Maybe V2D

   , field_stats              :: Stats
   , field_equipmentSlots     :: Set EquipmentSlot
   , field_unitType           :: Maybe UnitType

   , field_agentKind          :: AgentKind
   , field_scriptName         :: Maybe ScriptName
   } deriving (Generic)

data DelayedActionType
   = DelayedActionType_Attack EntityId AttackPower
   | DelayedActionType_FireProjectile V2D EntityId AttackPower

data DelayedAction = DelayedAction
   { field_timeLeft :: Duration
   , field_action   :: DelayedActionType
   } deriving (Generic)

data Agent = Agent
   { field_location           :: Location
   , field_velocity           :: Velocity
   , field_health             :: Health

   , field_animationState     :: AnimationState
   , field_animation          :: Animation

   , field_processOnUpdate    :: [EntityAction]
   , field_target             :: Maybe EntityId
   , field_timer              :: Timer

   , field_equipment          :: Equipment
   , field_debugFlags         :: EntityDebugFlags
   , field_updateOnce         :: Set UpdateOnce
   , field_collisionShape     :: Maybe CollisionShape
   , field_status             :: Set EntityStatus
   , field_attackMode         :: AttackMode
   , field_delayedActions     :: [DelayedAction]
   , field_baseStats          :: Stats
   , field_fullStats          :: Stats

   , field_runicLevel         :: RunicLevel
   , field_runicPoints        :: RunicPoints
   , field_maxRunicPoints     :: RunicPoints
   , field_selectedRune       :: Maybe RuneName

   , field_agentType          :: AgentType
   , field_script             :: Script

   , field_isMarked           :: Bool
   } deriving (Generic)
instance HasMaxSpeed Agent Speed where
    maxSpeed = ff#fullStats.ff#maxSpeed
instance HasAnimateWhenStopped Agent Bool where
    animateWhenStopped = agentType.ff#animateWhenStopped

-- Agent fields needed when displaying UI
data PlayerStatus = PlayerStatus
   { field_runicLevel         :: RunicLevel
   , field_runicPoints        :: RunicPoints
   , field_maxRunicPoints     :: RunicPoints
   , field_health             :: Health
   , field_selectedRune       :: Maybe RuneName
   , field_status             :: Set EntityStatus
   , field_attackMode         :: AttackMode
   , field_fullStats          :: Stats
   } deriving (Generic)

--------------------------------------------------------------------------------

instance ToJSON   UnitType where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON UnitType where parseJSON  = genericParseJSON  customOptionsJSON

instance ToJSON   AgentKind where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON AgentKind where parseJSON  = genericParseJSON  customOptionsJSON

instance ToJSON   AgentType where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON AgentType where parseJSON  = genericParseJSON  customOptionsJSON

instance Default UnitType
instance Default AgentType
instance Default Agent

instance GetZIndex Agent Word32 where
    get_zindex _ = toZIndex EntityZIndex_Vertical

agentType :: Lens' Agent AgentType
agentType = ff#agentType

updateOnce :: Lens' Agent (Set UpdateOnce)
updateOnce = ff#updateOnce

baseStats :: Lens' Agent Stats
baseStats = ff#baseStats

fullStats :: Lens' Agent Stats
fullStats = ff#fullStats

script :: Lens' Agent Script
script = ff#script

runicPoints :: Lens' Agent RunicPoints
runicPoints = ff#runicPoints

agentKind :: Lens' AgentType AgentKind
agentKind = ff#agentKind

scriptName :: Lens' AgentType (Maybe ScriptName)
scriptName = ff#scriptName

