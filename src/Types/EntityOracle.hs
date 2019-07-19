{-# Language GADTs #-}
module Types.EntityOracle where

import Delude
import Types.Entity.Agent
import Types.Entity.Reactivity
import Types.Equipment
import Types.Entity.Common
import Types.Entity.Passive
import Types.Entity.Animation
import Types.EntityAction (UseActionName)

data EntityQuery a where
     EntityQuery_Name           :: EntityQuery Text
     EntityQuery_Location       :: EntityQuery Location
     EntityQuery_Equipment      :: EntityQuery Equipment
     EntityQuery_PassiveType    :: EntityQuery PassiveType
     EntityQuery_Content        :: EntityQuery [EntityId]
     EntityQuery_Volume         :: EntityQuery Volume
     EntityQuery_MaxVolume      :: EntityQuery Volume
     EntityQuery_FittingSlots   :: EntityQuery (Set EquipmentSlot)
     EntityQuery_CollisionShape :: EntityQuery CollisionShape
     EntityQuery_Reactivity     :: EntityQuery (Map ReactivCategory ReactivValue)
     EntityQuery_ItemAnimation  :: EntityQuery AnimationName
     EntityQuery_BehindBody     :: EntityQuery Bool
     EntityQuery_Status         :: EntityQuery (Set EntityStatus)
     EntityQuery_PlayerStatus   :: EntityQuery PlayerStatus
     EntityQuery_Stats          :: EntityQuery Stats
     EntityQuery_UseActions     :: EntityQuery [UseActionName]
     EntityQuery_Owner          :: EntityQuery EntityId
