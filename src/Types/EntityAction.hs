{-# Language TemplateHaskell #-}
module Types.EntityAction where

import Delude
import qualified Prelude
import Types.Entity.Common
import Types.Entity.Animation (AnimationKind, Direction, Animation)

--------------------------------------------------------------------------------

data EntityDebugFlag
   = EntityDebugFlag_DrawPickupRange
   deriving (Show)

data AttackMode
   = AttackMode_Manual
   | AttackMode_Auto
   deriving (Eq, Show, Enum, Bounded)
instance Default AttackMode where def = AttackMode_Manual

data RuneType
   = RuneType_Offensive
   | RuneType_Defensive
   deriving (Show)

data PlayerAction
   = PlayerAction_SelectRune
   | PlayerAction_UpdateRune RuneType Bool
   | PlayerAction_SetAttackMode AttackMode
   deriving (Show)

data EntityValue
   = EntityValue_Location  Location
   | EntityValue_Direction Direction
   | EntityValue_Animation Animation
   | EntityValue_CenterOffset V2D
instance Show EntityValue where
    show = \case
        EntityValue_Location     l -> "Set: " <> show l
        EntityValue_Direction    d -> "Set: " <> show d
        EntityValue_Animation    _ -> "Set: <Animation>"
        EntityValue_CenterOffset o -> "Set: CenterOffset " <> show o

data EntityAction
   = EntityAction_SetMoveVector V2D
   | EntityAction_SetValue EntityValue
   | EntityAction_ToggleDebug EntityDebugFlag
   | EntityAction_RunAnimation AnimationKind
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
   -- tell entity to pass item [EntityId] to [EntityId]
   | EntityAction_PassItem EntityId EntityId
   -- tell entity to use item from inventory with [EntityId]
   | EntityAction_UseItem EntityId
   -- tell entity to mark itself as targeted by player
   | EntityAction_SelfMarkAsTarget
   -- tell entity to unmark itself as targeted by player
   | EntityAction_SelfUnmarkAsTarget
   -- tell entity to execute attack on market target
   | EntityAction_ExecuteAttack
   -- tell entity it was attacked
   | EntityAction_SelfAttacked AttackPower
   -- send player specific actions
   | EntityAction_PlayerAction PlayerAction
   -- tell entity to fire self as a projectile at target
   | EntityAction_SelfFiredAsProjectile Location V2D EntityId AttackPower
   -- tell entity to use self on provided target [EntityId]
   | EntityAction_SelfUseOn EntityId
   -- tell entity to heal self
   | EntityAction_SelfHeal Health
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
