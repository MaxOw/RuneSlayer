{-# Language TemplateHaskell #-}
module Types.Entity.Container where

import Delude

import Types.EntityAction
import Types.Entity.Common
import Types.Entity.ItemType

--------------------------------------------------------------------------------

data Container = Container
   { container_location        :: Maybe Location
   , container_owner           :: Maybe EntityId
   , container_processOnUpdate :: [EntityAction]
   , container_content         :: [EntityId]
   , container_containerType   :: ContainerType
   } deriving (Generic)
makeFieldsCustom ''Container
instance HasItemType Container ItemType where itemType = containerType.itemType

instance Default Container
