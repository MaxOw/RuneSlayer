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
import Data.Vector (Vector)

import Types.Debug (DebugFlag)
import Types.EntityAction as All
import Types.EntityOracle as All
import Types.EntitySum    as All
import Engine (RenderAction (..))
import Engine.Common.Types (BBox)
import Data.ArrayType (R, RW)
import Data.VectorIndex (VectorIndex)

import Types.ResourceManager (ResourceMap)
import Types.Entity.Common (EntityId, EntityKind) -- , Location)
import Engine.KDTree (KDTree)

--------------------------------------------------------------------------------

data EntityWithId = EntityWithId
   { entityWithId_entityId :: EntityId
   , entityWithId_entity   :: Entity
   }

-- EntityIndex Query Monad
newtype Q a = Q { runQ :: IO a } deriving (Functor, Applicative, Monad)
class MonadQ m where liftQ :: Q a -> m a
instance MonadQ Q  where liftQ = id
instance MonadQ IO where liftQ = runQ

type ViewRange = BBox Float
data EntityIndexT t = EntityIndex
   { entityIndex_lastId              :: Maybe EntityId
   , entityIndex_entities            :: VectorIndex t EntityWithId

   , entityIndex_staticIndex         :: HashSet EntityId
   , entityIndex_dynamicIndex        :: HashSet EntityId
   , entityIndex_activatedIndex      :: HashSet EntityId
   , entityIndex_staticLocationIndex :: KDTree EntityId
   -- , entityIndex_entities            :: HashMap EntityId Entity
   }

type EntityIndex   = EntityIndexT R
type EntityIndexIO = EntityIndexT RW

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
   , entityUpdate :: EntityContext -> (Maybe Entity, [DirectedEntityAction])
   , entityRender :: RenderContext -> RenderAction
   , entityOracle :: EntityOracle
   , entitySave   :: EntitySum
   , entityKind   :: EntityKind
   }

instance HasEntity Entity Entity where entity = id

makeFieldsCustom ''EntityIndexT
makeFieldsCustom ''EntityContext
makeFieldsCustom ''RenderContext


oracle :: Getter Entity EntityOracle
oracle = to entityOracle

makeFieldsCustom ''EntityWithId

data EntityParts p = EntityParts
   { makeActOn  :: p -> EntityAction -> p
   , makeUpdate :: p -> EntityContext -> (Maybe p, [DirectedEntityAction])
   , makeRender :: p -> RenderContext -> RenderAction
   , makeOracle :: p -> EntityOracle
   , makeSave   :: p -> EntitySum
   , makeKind   :: EntityKind
   }

