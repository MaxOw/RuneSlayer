{-# Language TemplateHaskell #-}
module Types.Entity.EntityType where

import Delude
import Types.Entity.ItemType

--------------------------------------------------------------------------------

data EntityType = EntityType
   { entityType_itemTypes :: HashMap ItemTypeName ItemType
   }
makeFieldsCustom ''EntityType

class EntityTypeName n where
    type EntityTypeResult n :: *
    lookupEntityTypeByName :: EntityType -> n -> Maybe (EntityTypeResult n)
