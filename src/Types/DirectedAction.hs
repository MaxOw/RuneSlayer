module Types.DirectedAction where

import Delude
import Types.Entity.Common
import Types.EntityAction
import Types.Entity.Passive    (PassiveTypeName)
import Types.Entity.Agent      (AgentTypeName, ScriptName)
import Types.Entity.Projectile (Projectile)
import Types.Entity.Effect     (EffectKind)

--------------------------------------------------------------------------------

data SpawnEntity
   = SpawnEntity_Passive PassiveTypeName
   | SpawnEntity_Agent   AgentTypeName

   | SpawnEntity_Effect  EffectKind
   | SpawnEntity_Projectile Projectile

data SpawnEntityOpts = SpawnEntityOpts
   { field_actions     :: [EntityAction]
   , field_tagAsCamera :: Bool
   } deriving (Generic)
instance Default SpawnEntityOpts

data Message
   = Message_Info Text
   | Message_HitEffect Location Health

data WorldAction
   = WorldAction_SpawnEntity SpawnEntity SpawnEntityOpts
   -- Open inventory/contents inspection window of [EntityId] for player
   | WorldAction_InspectContent EntityId
   | WorldAction_StartDialog EntityId
   | WorldAction_Message Message
   | WorldAction_MarkTarget (Maybe EntityId)
   | WorldAction_RegisterNPC ScriptName EntityId
   | WorldAction_GameOver

--------------------------------------------------------------------------------

data DirectedAction
   = DirectedAtEntity DirectedEntityAction
   | DirectedAtWorld  WorldAction

--------------------------------------------------------------------------------

directAtEntity :: EntityId -> EntityAction -> DirectedAction
directAtEntity eid ea = DirectedAtEntity $ DirectedEntityAction eid ea

directAtWorld :: WorldAction -> DirectedAction
directAtWorld = DirectedAtWorld

--------------------------------------------------------------------------------

tagAsCamera :: Lens' SpawnEntityOpts Bool
tagAsCamera = ff#tagAsCamera
