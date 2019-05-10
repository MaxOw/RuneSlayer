{-# Language GADTs #-}
module Types.EntityOracle where

import Delude
import Types.Entity.Player
import Types.Entity.Reactivity
import Types.Equipment
import Types.Entity.Common
import Types.Entity.Item
import Types.Entity.ZIndex
import Types.Entity.Animation

data EntityQuery a where
     EntityQuery_Name           :: EntityQuery Text
     EntityQuery_Location       :: EntityQuery Location
     EntityQuery_Equipment      :: EntityQuery Equipment
     EntityQuery_ItemKind       :: EntityQuery ItemKind
     EntityQuery_Content        :: EntityQuery [EntityId]
     EntityQuery_Volume         :: EntityQuery Volume
     EntityQuery_MaxVolume      :: EntityQuery Volume
     EntityQuery_FittingSlots   :: EntityQuery (Set EquipmentSlot)
     EntityQuery_Zindex         :: EntityQuery EntityZIndex
     EntityQuery_CollisionShape :: EntityQuery CollisionShape
     EntityQuery_Reactivity     :: EntityQuery (Map ReactivCategory ReactivValue)
     EntityQuery_ItemAnimation  :: EntityQuery AnimationName
     EntityQuery_Status         :: EntityQuery (Set EntityStatus)
     EntityQuery_PlayerStatus   :: EntityQuery PlayerStatus
