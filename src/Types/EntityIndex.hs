module Types.EntityIndex where

import Delude
import Engine.Common.Types (Size)
import Types.Entity.Common
import Types.Collider (Shape, CollideWith)

import Data.VectorIndex (VectorIndex)
import Data.SpatialIndex (SpatialIndex)
import Data.FullMap (FullMap)

--------------------------------------------------------------------------------

data StaticShape = StaticShape
   { field_center   :: V2 Float
   , field_shape    :: Shape
   , field_mask     :: BitSet32 CollideWith
   } deriving (Generic)
instance Eq  StaticShape where (==) = (==) `on` (view center)
instance Ord StaticShape where compare = compare `on` (view center)
instance Hashable StaticShape where hashWithSalt s = hashWithSalt s . view center

data EntityIndexTag
   = EntityIndexTag_Camera
   | EntityIndexTag_Focus
   deriving (Eq, Ord)

data EntityIndexConfig = EntityIndexConfig
   { field_size :: Size Float
   } deriving (Generic)
instance HasSize EntityIndexConfig (Size Float)

data EntityIndexT a = EntityIndex
   { field_lastId              :: IORef (Maybe EntityId)
   , field_entities            :: VectorIndex (EntityWithIdT a)

   , field_dynamicIndex        :: IORef (HashSet EntityId)
   , field_activatedSet        :: IORef (HashSet EntityId)
   , field_spatialIndex        :: FullMap EntityKind (SpatialIndex EntityId)
   , field_staticShapes        :: SpatialIndex StaticShape
   , field_tags                :: IORef (Map EntityIndexTag EntityId)
   } deriving (Generic)

activatedSet :: Lens' (EntityIndexT a) (IORef (HashSet EntityId))
activatedSet = ff#activatedSet

--------------------------------------------------------------------------------

instance Default EntityIndexConfig where
    def = EntityIndexConfig
        { field_size = pure 30
        }
