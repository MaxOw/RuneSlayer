module Types.Entity
    ( module All
    -- , Entity (..), EntityWithId (..)
    -- , EntityIndex (..), EntityContext (..)
    -- , RenderContext (..), RenderAction (..)
    -- , EntityParts (..)

    , module Types.Entity
    ) where

import Delude

import Types.Debug (DebugFlag)
import Types.DirectedAction as All
import Types.EntityAction   as All
import Types.EntityOracle   as All
import Types.EntitySum      as All
import Engine (RenderAction (..))
import Engine.Common.Types (BBox)

import Types.ResourceManager (Resources)
import Types.Entity.Common as All (EntityId, EntityWithIdT(..), EntityKind)

import Types.EntityIndex as All

--------------------------------------------------------------------------------

type EntityWithId = EntityWithIdT Entity
type EntityIndex  = EntityIndexT Entity

-- EntityIndex Query Monad
newtype Q a = Q { unQ :: IO a } deriving (Functor, Applicative, Monad)
runQ :: MonadIO m => Q a -> m a
runQ = liftIO . unQ
class Monad m => MonadQ m where liftQ :: Q a -> m a
instance MonadQ IO where liftQ = runQ
instance MonadQ Q  where liftQ = id
instance MonadQ (StateT us IO) where liftQ = runQ
instance MonadQ (StateT us Q) where liftQ = lift

type RangeBBox = BBox Float

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
   , entityOracle :: forall a. EntityQuery a -> Maybe a
   , entitySave   :: Maybe EntitySum
   , entityKind   :: EntityKind
   }

instance HasEntity Entity Entity where entity = id

data EntityParts p = EntityParts
   { makeActOn  :: p -> EntityAction -> p
   , makeUpdate :: p -> EntityContext -> Q (Maybe p, [DirectedAction])
   , makeRender :: p -> RenderContext -> RenderAction
   , makeOracle :: p -> forall a. EntityQuery a -> Maybe a
   , makeSave   :: p -> Maybe EntitySum
   , makeKind   :: EntityKind
   }

