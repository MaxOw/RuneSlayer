module Types.DirectedAction where

import Delude
import Types.Entity.Common
import Types.EntityAction
import Types.Entity.Item
import Types.Entity.Unit
import Types.Entity.Animation (Direction)
import Types.Entity.Effect (EffectKind)
-- import Types.Entity.EntityType

--------------------------------------------------------------------------------

data Spawn a = Spawn
   { field_location  :: Location
   , field_direction :: Maybe Direction
   , field_name      :: a
   } deriving (Generic)
instance Default a => Default (Spawn a)

data SpawnEntity
   = SpawnEntity_Item   (Spawn ItemTypeName)
   | SpawnEntity_Unit   (Spawn UnitTypeName)
   | SpawnEntity_Effect Location EffectKind

data WorldAction
   = WorldAction_SpawnEntity SpawnEntity

--------------------------------------------------------------------------------

data DirectedAction
   = DirectedAtEntity DirectedEntityAction
   | DirectedAtWorld  WorldAction

--------------------------------------------------------------------------------

directAtEntity :: EntityId -> EntityAction -> DirectedAction
directAtEntity eid ea = DirectedAtEntity $ DirectedEntityAction eid ea

directAtWorld :: WorldAction -> DirectedAction
directAtWorld = DirectedAtWorld

