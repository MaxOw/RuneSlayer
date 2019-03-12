{-# Language TemplateHaskell #-}
module Types.DirectedAction where

import Delude
import Types.Entity.Common
import Types.EntityAction
import Types.Entity.ItemType
-- import Types.Entity.EntityType

--------------------------------------------------------------------------------

data SpawnItem = SpawnItem
   { spawnItem_location :: Location
   , spawnItem_itemType :: ItemTypeName
   } deriving (Generic)
makeFieldsCustom ''SpawnItem
instance Default SpawnItem

data SpawnEntity
   = SpawnEntity_Item SpawnItem

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

