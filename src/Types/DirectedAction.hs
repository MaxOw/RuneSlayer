module Types.DirectedAction where

import Delude
import Types.Entity.Common
import Types.EntityAction
import Types.Entity.Item
import Types.Entity.Unit
import Types.Entity.Projectile (Projectile)
import Types.Entity.Effect (EffectKind)

--------------------------------------------------------------------------------

data SpawnEntity
   = SpawnEntity_Item   ItemTypeName
   | SpawnEntity_Unit   UnitTypeName
   | SpawnEntity_Effect Location EffectKind
   | SpawnEntity_Projectile Projectile

data WorldAction
   = WorldAction_SpawnEntity SpawnEntity [EntityAction]

--------------------------------------------------------------------------------

data DirectedAction
   = DirectedAtEntity DirectedEntityAction
   | DirectedAtWorld  WorldAction

--------------------------------------------------------------------------------

directAtEntity :: EntityId -> EntityAction -> DirectedAction
directAtEntity eid ea = DirectedAtEntity $ DirectedEntityAction eid ea

directAtWorld :: WorldAction -> DirectedAction
directAtWorld = DirectedAtWorld

