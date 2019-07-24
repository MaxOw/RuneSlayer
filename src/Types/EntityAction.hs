{-# Language TemplateHaskell #-}
module Types.EntityAction where

import Delude
import qualified Prelude
import Types.Entity.Common
import Types.Entity.PassiveType (LoadoutEntry, UseActionName)
import Types.Entity.Animation (AnimationKind, Direction, Animation)
import Types.Equipment (EquipmentSlot)

--------------------------------------------------------------------------------

data EntityDebugFlag
   = EntityDebugFlag_DrawPickupRange
   deriving (Generic, Show)

data AttackMode
   = AttackMode_Manual
   | AttackMode_Auto
   deriving (Generic, Eq, Show, Enum, Bounded)
instance Default AttackMode where def = AttackMode_Manual

data RuneType
   = RuneType_Offensive
   | RuneType_Defensive
   deriving (Generic, Show)

data PlayerAction
   = PlayerAction_SelectRune
   | PlayerAction_UpdateRune RuneType Bool
   | PlayerAction_SetAttackMode AttackMode
   deriving (Generic, Show)

data UseAction
   = UseAction_Open
   | UseAction_InspectContents
   | UseAction_Close
   deriving (Eq, Ord, Show, Generic)
instance ToJSON UseAction where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON UseAction where
    parseJSON = genericParseJSON customOptionsJSON
instance ToJSONKey   UseAction where toJSONKey   = defaultToJSONKey
instance FromJSONKey UseAction where fromJSONKey = defaultFromJSONKey

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
   -- tell entity it was attacked
   | EntityAction_SelfAttacked AttackPower
   -- send player specific actions
   | EntityAction_PlayerAction PlayerAction
   -- tell entity to fire self as a projectile at target
   | EntityAction_SelfFiredAsProjectile Location V2D EntityId AttackPower
   -- tell entity to heal self
   | EntityAction_SelfHeal Health
   -- tell entity to perform given [UseActionName] at [EntityId]
   | EntityAction_UseAction UseActionName EntityId

   -- tell entity to create and equip/contain given loadout.
   | EntityAction_AddLoadout [LoadoutEntry]
   deriving (Show, Generic)
makePrisms ''EntityAction

instance FromJSON EntityDebugFlag where parseJSON = genericParseJSON customOptionsJSON
instance FromJSON EntityValue  where parseJSON = genericParseJSON customOptionsJSON
instance FromJSON AttackMode   where parseJSON = genericParseJSON customOptionsJSON
instance FromJSON RuneType     where parseJSON = genericParseJSON customOptionsJSON
instance FromJSON PlayerAction where parseJSON = genericParseJSON customOptionsJSON
instance FromJSON EntityAction where parseJSON = genericParseJSON customOptionsJSON

data DirectedEntityAction = DirectedEntityAction
   { field_entityId :: EntityId
   , field_action   :: EntityAction
   } deriving (Generic)
instance HasEntityId DirectedEntityAction EntityId

instance Show DirectedEntityAction where
    show (DirectedEntityAction i a) =
        show i <> " => " <> show a -- <> "."
