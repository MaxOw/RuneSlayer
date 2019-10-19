{-# Language TemplateHaskell #-}
module Types.Entity.Passive
    ( module Types.Entity.Passive
    , module Types.Entity.PassiveType
    ) where

import Delude

import Types.EntityAction
import Types.Entity.PassiveType
import Types.Entity.Animation
import Types.Entity.Common
import Types.Collider (Shape, CollideWith)
-- import Types.Entity.ZIndex
import Types.Entity.Animation (Direction)
import Data.BitSet (BitSet32)
import Entity.HasField

--------------------------------------------------------------------------------

data Passive = Passive
   { field_location        :: Maybe Location
   , field_owner           :: Maybe EntityId
   , field_passiveType     :: PassiveType
   , field_processOnUpdate :: [EntityAction]
   , field_content         :: [EntityId]
   , field_contentVolume   :: Volume
   , field_direction       :: Maybe Direction
   , field_animationState  :: AnimationState
   , field_animation       :: Animation
   , field_collisionShape  :: Maybe Shape
   } deriving (Generic)
instance HasStandingWeight Passive Weight where
    standingWeight = passiveType.ff#standingWeight
instance HasCollisionBits Passive (BitSet32 CollideWith) where
    collisionBits = passiveType.ff#collisionBits

--------------------------------------------------------------------------------

instance Default Passive
instance GetZIndex Passive Word32 where get_zindex x = x^.passiveType.zindex

--------------------------------------------------------------------------------

passiveKind :: Lens' PassiveType (Set PassiveKind)
passiveKind = ff#passiveKind

allowKinds :: Lens' ContainerType (Set PassiveKind)
allowKinds = ff#allowKinds

behindBody :: Lens' PassiveType (Maybe Bool)
behindBody = ff#behindBody

showCount :: Lens' ContainerType Bool
showCount = ff#showCount

