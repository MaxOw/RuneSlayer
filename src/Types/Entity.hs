{-# Language TemplateHaskell #-}
module Types.Entity
    ( module All
    , Entity (..), EntityWithId (..)
    , EntityIndex (..), EntityContext (..)
    , EntityParts (..), RenderAction (..)

    , oracle
    ) where

import Delude

import Types.EntityAction as All
import Types.EntityOracle as All
import Types.EntitySum    as All
import Engine (RenderAction (..))

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

data Entity = Entity
   { entityActOn  :: EntityAction -> Entity
   , entityUpdate :: EntityContext -> (Maybe Entity, [DirectedEntityAction])
   , entityRender :: RenderAction
   , entityOracle :: EntityOracle
   , entitySave   :: EntitySum
   }

instance HasEntity Entity Entity where entity = id

makeFieldsCustom ''EntityContext


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
   , makeRender :: p -> RenderAction
   , makeOracle :: p -> EntityOracle
   , makeSave   :: p -> EntitySum
   }

