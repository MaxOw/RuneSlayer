module Types.EntityIndex where

import Delude
import Engine.Common.Types (Size)
import Types.Entity.Common

import Data.VectorIndex (VectorIndex)
import Data.SpatialIndex (SpatialIndex)
import Data.FullMap (FullMap)

--------------------------------------------------------------------------------

data EntityIndexConfig = EntityIndexConfig
   { field_size :: Size Float
   } deriving (Generic)

data EntityIndexT a = EntityIndex
   { field_lastId              :: IORef (Maybe EntityId)
   , field_entities            :: VectorIndex (EntityWithIdT a)

   , field_dynamicIndex        :: IORef (HashSet EntityId)
   , field_activatedList       :: IORef [EntityId]
   , field_spatialIndex        :: FullMap EntityKind (SpatialIndex EntityId)
   } deriving (Generic)

--------------------------------------------------------------------------------

instance Default EntityIndexConfig where
    def = EntityIndexConfig
        { field_size = pure 30
        }
