{-# Language TemplateHaskell #-}
module Types.EntityAction where

import Delude
import qualified Prelude
import Types.Entity.Common
import Types.Entity.Animation (AnimationKind)

--------------------------------------------------------------------------------

data EntityDebugFlag
   = EntityDebugFlag_DrawPickupRange
   deriving (Show)

data EntityAction
   = EntityAction_SetMoveVector V2D
   | EntityAction_ToggleDebug EntityDebugFlag
   | EntityAction_DebugRunAnimation AnimationKind
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
   -- tell entity to drop selected [EntityId]
   | EntityAction_DropItem EntityId
   -- tell entity to that it's child container wants to drop selected [EntityId]
   | EntityAction_OwnerDropItem EntityId
   -- tell entity to mark itself as targeted by player
   | EntityAction_SelfMarkAsTarget
   -- tell entity to unmark itself as targeted by player
   | EntityAction_SelfUnmarkAsTarget
   -- tell entity to execute attack on market target
   | EntityAction_ExecuteAttack
   -- tell entity it was attacked
   | EntityAction_SelfAttacked AttackPower
   --
   | EntityAction_LoadOffensiveSlot
   deriving (Show)
makePrisms ''EntityAction

data DirectedEntityAction = DirectedEntityAction
   { field_entityId :: EntityId
   , field_action   :: EntityAction
   } deriving (Generic)
instance HasEntityId DirectedEntityAction EntityId


instance Show DirectedEntityAction where
    show (DirectedEntityAction i a) =
        show i <> " => " <> show a -- <> "."
