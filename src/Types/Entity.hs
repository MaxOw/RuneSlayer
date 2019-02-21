{-# Language TemplateHaskell #-}
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
import Types.EntityAction as All
import Types.EntityOracle as All
import Types.EntitySum    as All
import Engine (RenderAction (..))
import Engine.Common.Types (BBox)
import Data.VectorIndex (VectorIndex)

import Types.ResourceManager (ResourceMap)
import Types.Entity.Common (EntityId, EntityKind) -- , Location)
import Data.SpatialIndex (SpatialIndex)
import Data.FullMap (FullMap)

--------------------------------------------------------------------------------

data EntityWithId = EntityWithId
   { entityWithId_entityId :: EntityId
   , entityWithId_entity   :: Entity
   }

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
   { entityIndex_lastId              :: IORef (Maybe EntityId)
   , entityIndex_entities            :: VectorIndex EntityWithId

   , entityIndex_dynamicIndex        :: IORef (HashSet EntityId)
   , entityIndex_activatedList       :: IORef [EntityId]
   , entityIndex_spatialIndex        :: FullMap EntityKind (SpatialIndex EntityId)
   }

data EntityContext = EntityContext
   { entityContext_entities   :: EntityIndex
   , entityContext_selfId     :: EntityId
   , entityContext_frameCount :: Word32
   }

data RenderContext = RenderContext
   { renderContext_resources  :: ResourceMap
   , renderContext_debugFlags :: Set DebugFlag
   }

data Entity = Entity
   { entityActOn  :: EntityAction -> Entity
   , entityUpdate :: EntityContext -> Q (Maybe Entity, [DirectedEntityAction])
   , entityRender :: RenderContext -> RenderAction
   , entityOracle :: EntityOracle
   , entitySave   :: EntitySum
   , entityKind   :: EntityKind
   }

instance HasEntity Entity Entity where entity = id

makeFieldsCustom ''EntityIndex
makeFieldsCustom ''EntityContext
makeFieldsCustom ''RenderContext


oracle :: Getter Entity EntityOracle
oracle = to entityOracle

makeFieldsCustom ''EntityWithId

data EntityParts p = EntityParts
   { makeActOn  :: p -> EntityAction -> p
   , makeUpdate :: p -> EntityContext -> Q (Maybe p, [DirectedEntityAction])
   , makeRender :: p -> RenderContext -> RenderAction
   , makeOracle :: p -> EntityOracle
   , makeSave   :: p -> EntitySum
   , makeKind   :: EntityKind
   }

