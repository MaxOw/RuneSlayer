module Types.Entity.EntityType where

import Delude
import Types.Entity.ItemType

--------------------------------------------------------------------------------

data EntityType = EntityType
   { field_itemTypes :: HashMap ItemTypeName ItemType
   }


class EntityTypeName n where
    type EntityTypeResult n :: *
    lookupEntityTypeByName :: EntityType -> n -> Maybe (EntityTypeResult n)
