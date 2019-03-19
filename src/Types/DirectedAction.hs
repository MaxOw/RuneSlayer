{-# Language TemplateHaskell #-}
module Types.DirectedAction where

import Delude
import Types.Entity.Common
import Types.EntityAction
import Types.Entity.ItemType
import Types.Entity.Unit
-- import Types.Entity.EntityType

--------------------------------------------------------------------------------

data Spawn a = Spawn
   { spawnItem_location :: Location
   , spawnItem_name     :: a
   } deriving (Generic)
makeFieldsCustom ''Spawn
instance Default a => Default (Spawn a)

data SpawnEntity
   = SpawnEntity_Item (Spawn ItemTypeName)
   | SpawnEntity_Unit (Spawn UnitTypeName)

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

