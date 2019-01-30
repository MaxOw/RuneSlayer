{-# Language TemplateHaskell #-}
module Types.Entity
    ( module All
    , Entity (..), EntityWithId (..)
    , EntityIndex (..), EntityContext (..)
    , RenderContext (..), RenderAction (..)
    , EntityParts (..)

    , oracle
    ) where

import Delude

import Types.EntityAction as All
import Types.EntityOracle as All
import Types.EntitySum    as All
import Engine (RenderAction (..))

import Types.ResourceManager (ResourceMap)
import Types.Entity.Common (EntityId) -- , Location)

--------------------------------------------------------------------------------

data EntityIndex = EntityIndex
   { entityIndexLastId   :: EntityId
   , entityIndexEntities :: HashMap EntityId Entity
-- , entityIndexLocation :: SPTree Location EntityId
   }

data EntityContext = EntityContext
   { entityContext_entities   :: EntityIndex
   , entityContext_selfId     :: EntityId
   , entityContext_frameCount :: Word32
   }

data RenderContext = RenderContext
   { renderContext_resources :: ResourceMap
   }

data Entity = Entity
   { entityActOn  :: EntityAction -> Entity
   , entityUpdate :: EntityContext -> (Maybe Entity, [DirectedEntityAction])
   , entityRender :: RenderContext -> RenderAction
   , entityOracle :: EntityOracle
   , entitySave   :: EntitySum
   }

instance HasEntity Entity Entity where entity = id

makeFieldsCustom ''EntityContext
makeFieldsCustom ''RenderContext


oracle :: Getter Entity EntityOracle
oracle = to entityOracle

data EntityWithId = EntityWithId
   { entityWithId_entityId :: EntityId
   , entityWithId_entity   :: Entity
   }
makeFieldsCustom ''EntityWithId

data EntityParts p = EntityParts
   { makeActOn  :: p -> EntityAction -> p
   , makeUpdate :: p -> EntityContext -> (Maybe p, [DirectedEntityAction])
   , makeRender :: p -> RenderContext -> RenderAction
   , makeOracle :: p -> EntityOracle
   , makeSave   :: p -> EntitySum
   }

