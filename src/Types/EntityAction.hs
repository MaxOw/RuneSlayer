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
   | EntityAction_SelfAddedBy EntityId
   -- tell entity that it was passed to [EntityId]
   | EntityAction_SelfPassedTo EntityId
   -- tell entity that it should add [EntityId] to inventory
   | EntityAction_AddItem EntityId
   -- tell entity that it got dropped at [Location]
   | EntityAction_SelfDroppedAt Location
   -- tell entity to drop all items
   | EntityAction_DropAllItems
   -- tell entity to drop selected item
   | EntityAction_DropItem EntityId
   deriving (Show)
makePrisms ''EntityAction

data DirectedEntityAction = DirectedEntityAction
   { directedEntity_entityId :: EntityId
   , directedEntity_action   :: EntityAction
   }
makeFieldsCustom ''DirectedEntityAction

instance Show DirectedEntityAction where
    show (DirectedEntityAction i a) =
        show i <> " => " <> show a -- <> "."
