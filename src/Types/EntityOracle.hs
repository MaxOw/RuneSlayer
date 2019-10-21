{-# Language GADTs #-}
module Types.EntityOracle where

import Delude
import Types.Entity.Agent
import Types.Entity.Reactivity
import Types.Collider (Shape, CollideWith)
import Types.Equipment
import Types.Entity.Common
import Types.Entity.Passive
import Types.Entity.Animation

data EntityQuery a where
     EntityQuery_Name               :: EntityQuery Text
     EntityQuery_DisplayName        :: EntityQuery Text
     EntityQuery_Location           :: EntityQuery Location
     EntityQuery_Equipment          :: EntityQuery Equipment
     EntityQuery_PassiveType        :: EntityQuery PassiveType
     EntityQuery_AgentType          :: EntityQuery AgentType
     EntityQuery_Content            :: EntityQuery [EntityId]
     EntityQuery_Volume             :: EntityQuery Volume
     EntityQuery_MaxVolume          :: EntityQuery Volume
     EntityQuery_StandingWeight     :: EntityQuery Weight
     EntityQuery_FittingSlots       :: EntityQuery (Set EquipmentSlot)
     EntityQuery_CollisionShape     :: EntityQuery Shape
     EntityQuery_CollisionBits      :: EntityQuery (BitSet32 CollideWith)
     EntityQuery_Reactivity         :: EntityQuery (Map ReactivCategory ReactivValue)
     EntityQuery_ItemAnimation      :: EntityQuery AnimationName
     EntityQuery_BehindBody         :: EntityQuery Bool
     EntityQuery_Status             :: EntityQuery (Set EntityStatus)
     EntityQuery_PlayerStatus       :: EntityQuery PlayerStatus
     EntityQuery_Stats              :: EntityQuery Stats
     EntityQuery_Interactions       :: EntityQuery [InteractionName]
     EntityQuery_PrimaryInteraction :: EntityQuery InteractionName
     EntityQuery_Owner              :: EntityQuery EntityId
     EntityQuery_LabelOffset        :: EntityQuery V2D
