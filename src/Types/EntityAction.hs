{-# Language TemplateHaskell #-}
module Types.EntityAction where

import Delude
import qualified Prelude
import Types.Entity.Common

--------------------------------------------------------------------------------

data DebugFlag
   = DebugFlag_DrawPickupRange
   deriving (Show)

data EntityAction
   = EntityAction_SetMoveVector V2D
   | EntityAction_ToggleDebug DebugFlag
   -- tell entity that it was picked up by [EntityId]
   | EntityAction_PickItemBy EntityId
   -- tell entity that it should add [EntityId] to inventory
   | EntityAction_AddToInventory EntityId
   -- tell entity that it got dropped at [Location]
   | EntityAction_DropItemAt Location
   -- tell entity to drop all items
   | EntityAction_DropAllItems
   deriving (Show)
makePrisms ''EntityAction

data DirectedEntityAction = DirectedEntityAction
   { directedEntity_entityId :: EntityId
   , directedEntity_action   :: EntityAction
   }
makeFieldsCustom ''DirectedEntityAction

instance Show DirectedEntityAction where
    show (DirectedEntityAction i a) =
        "DirectedEntityAction " <> show i <> " (" <> show a <> ")"
