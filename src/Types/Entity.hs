module Types.Entity
    ( module All
    -- , Entity (..), EntityWithId (..)
    -- , EntityIndex (..), EntityContext (..)
    -- , RenderContext (..), RenderAction (..)
    -- , EntityParts (..)

    , module Types.Entity
    ) where

import Delude
import qualified Control.Monad.Trans.State.Lazy as Lazy

import Types.Debug (DebugFlag)
import Types.DirectedAction as All
import Types.EntityAction   as All
import Types.EntityOracle   as All
import Types.EntitySum      as All
import Engine (RenderAction (..))
import Engine.Common.Types (BBox)
import Data.VectorIndex (VectorIndex)

import Types.ResourceManager (Resources)
import Types.Entity.Common (EntityId, EntityKind) -- , Location)
import Data.SpatialIndex (SpatialIndex)
import Data.FullMap (FullMap)

--------------------------------------------------------------------------------

data EntityWithId = EntityWithId
   { field_entityId :: EntityId
   , field_entity   :: Entity
   } deriving (Generic)
instance HasEntityId EntityWithId EntityId
instance HasEntity   EntityWithId Entity

-- EntityIndex Query Monad
newtype Q a = Q { unQ :: IO a } deriving (Functor, Applicative, Monad)
runQ :: MonadIO m => Q a -> m a
runQ = liftIO . unQ
class Monad m => MonadQ m where liftQ :: Q a -> m a
instance MonadQ IO where liftQ = runQ
instance MonadQ Q  where liftQ = id
instance MonadQ (Lazy.StateT us IO) where liftQ = runQ
instance MonadQ (StateT us Q) where liftQ = lift

type RangeBBox = BBox Float
data EntityIndex = EntityIndex
   { field_lastId              :: IORef (Maybe EntityId)
   , field_entities            :: VectorIndex EntityWithId

   , field_dynamicIndex        :: IORef (HashSet EntityId)
   , field_activatedList       :: IORef [EntityId]
   , field_spatialIndex        :: FullMap EntityKind (SpatialIndex EntityId)
   } deriving (Generic)

data EntityContext = EntityContext
   { field_entities   :: EntityIndex
   , field_selfId     :: EntityId
   , field_frameCount :: Word32
   , field_resources  :: Resources
   } deriving (Generic)
instance HasResources EntityContext Resources

data RenderContext = RenderContext
   { field_resources  :: Resources
   , field_debugFlags :: Set DebugFlag
   } deriving (Generic)
instance HasResources RenderContext Resources

data Entity = Entity
   { entityActOn  :: EntityAction -> Entity
   , entityUpdate :: EntityContext -> Q (Maybe Entity, [DirectedAction])
   , entityRender :: RenderContext -> RenderAction
   , entityOracle :: EntityOracle
   , entitySave   :: EntitySum
   , entityKind   :: EntityKind
   }

instance HasEntity Entity Entity where entity = id






oracle :: Getter Entity EntityOracle
oracle = to entityOracle



data EntityParts p = EntityParts
   { makeActOn  :: p -> EntityAction -> p
   , makeUpdate :: p -> EntityContext -> Q (Maybe p, [DirectedAction])
   , makeRender :: p -> RenderContext -> RenderAction
   , makeOracle :: p -> EntityOracle
   , makeSave   :: p -> EntitySum
   , makeKind   :: EntityKind
   }

