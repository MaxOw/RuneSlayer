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

import Types.ResourceManager (ResourceMap)
import Types.Entity.Common (EntityId, EntityKind) -- , Location)
import Engine.KDTree (KDTree)

--------------------------------------------------------------------------------

data EntityWithId = EntityWithId
   { entityWithId_entityId :: EntityId
   , entityWithId_entity   :: Entity
   }

type ViewRange = BBox Float
data EntityIndex = EntityIndex
   { entityIndex_lastId              :: Maybe EntityId
   , entityIndex_emptyOffsets        :: [Int]
   , entityIndex_entities            :: Vector (Maybe EntityWithId)
   , entityIndex_staticIndex         :: HashSet EntityId
   , entityIndex_dynamicIndex        :: HashSet EntityId
   , entityIndex_activatedIndex      :: HashSet EntityId
   , entityIndex_staticLocationIndex :: KDTree EntityId
   -- , entityIndex_entities            :: HashMap EntityId Entity
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
   , entityUpdate :: EntityContext -> (Maybe Entity, [DirectedEntityAction])
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
   , makeUpdate :: p -> EntityContext -> (Maybe p, [DirectedEntityAction])
   , makeRender :: p -> RenderContext -> RenderAction
   , makeOracle :: p -> EntityOracle
   , makeSave   :: p -> EntitySum
   , makeKind   :: EntityKind
   }

