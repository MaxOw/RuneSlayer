{-# Language TemplateHaskell #-}
module Types.EntityAction where

import Delude
import qualified Prelude
import Types.Entity.Common
import Types.Entity.PassiveType (LoadoutEntry, PassiveTypeName, InteractionName)
import Types.Entity.Animation (AnimationKind, Direction, Animation)
import Types.Equipment (EquipmentSlot)
import Types.Skills.Runes (RuneSet)

--------------------------------------------------------------------------------

data Spawn n a = Spawn
   { field_name    :: n
   , field_actions :: [a]
   } deriving (Generic, Show, Functor, Foldable, Traversable)

data EntityDebugFlag
   = EntityDebugFlag_DrawPickupRange
   deriving (Generic, Show)

data AttackMode
   = AttackMode_Manual
   | AttackMode_Auto
   deriving (Generic, Eq, Ord, Show, Enum, Bounded)
instance Default AttackMode where def = AttackMode_Manual

data PlayerAction
   = PlayerAction_SelectRune
   | PlayerAction_AddRunes RuneSet
   | PlayerAction_UpdateRune Bool
   | PlayerAction_SetAttackMode AttackMode
   deriving (Generic, Show)

data EntityValue
   = EntityValue_Location  Location
   | EntityValue_Direction Direction
   | EntityValue_Animation Animation
   deriving (Generic)
instance Show EntityValue where
    show = \case
        EntityValue_Location     l -> "Set: " <> show l
        EntityValue_Direction    d -> "Set: " <> show d
        EntityValue_Animation    _ -> "Set: <Animation>"

data DialogAction
   = DialogAction_Start    -- ^ Start a conversation with given NPC.
   | DialogAction_NextPage -- ^ Move dialog progression to next page.
   deriving (Show, Generic)

data FiredProjectileOpts = FiredProjectileOpts
   { field_location    :: Location
   , field_direction   :: V2D
   , field_source      :: Maybe EntityId
   , field_target      :: EntityId
   , field_attackPower :: AttackPower
   } deriving (Show, Generic)

data EntityAction
   = EntityAction_SetMoveVector V2D
   | EntityAction_SetValue EntityValue
   | EntityAction_ToggleDebug EntityDebugFlag
   | EntityAction_RunAnimation AnimationKind

-- Item ownership/moving/dropping actions
--------------------------------------------------------------------------------
   -- tell entity to add item [EntityId] (possibly at a specified slot)
   | EntityAction_AddItem    EntityId (Maybe EquipmentSlot)
   -- tell entity to remove [EntityId] from its content
   | EntityAction_RemoveItem EntityId
   -- tell entity to pass self to
   -- target: Nothing = Drop, slot: Nothing = Wherever
   | EntityAction_SelfPassTo (Maybe EntityId) (Maybe EquipmentSlot)
--------------------------------------------------------------------------------

   -- tell entity to use item from inventory with [EntityId]
   | EntityAction_UseItem EntityId
   -- tell entity to mark itself as targeted by player
   | EntityAction_SelfMarkAsTarget
   -- tell entity to unmark itself as targeted by player
   | EntityAction_SelfUnmarkAsTarget
   -- tell entity to execute attack on market target
   | EntityAction_ExecuteAttack
   -- tell entity it was attacked with [AttackPower] possibly by [EntityId]
   | EntityAction_SelfAttacked AttackPower (Maybe EntityId)
   -- send player specific actions
   | EntityAction_PlayerAction PlayerAction
   -- tell entity to fire self as a projectile at target
   | EntityAction_SelfFiredAsProjectile FiredProjectileOpts
   -- tell entity to heal self
   | EntityAction_SelfHeal Health
   -- tell entity to perform given [InteractionName] with [EntityId]
   | EntityAction_Interact InteractionName EntityId

   -- tell entity to create and equip/contain given loadout.
   | EntityAction_AddLoadout [LoadoutEntry (Spawn PassiveTypeName EntityAction)]

   -- send entity DialogAction (actions used when communicating with NPCs)
   | EntityAction_Dialog DialogAction
   deriving (Show, Generic)
makePrisms ''EntityAction

data DirectedEntityAction = DirectedEntityAction
   { field_entityId :: EntityId
   , field_action   :: EntityAction
   } deriving (Generic)
instance HasEntityId DirectedEntityAction EntityId

instance Show DirectedEntityAction where
    show (DirectedEntityAction i a) = show i <> " => " <> show a
