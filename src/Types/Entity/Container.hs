{-# Language TemplateHaskell #-}
module Types.Entity.Container where

import Delude

import Types.EntityAction
import Types.Entity.Common
import Types.Entity.ItemType
import Types.Entity.ZIndex

--------------------------------------------------------------------------------

data Container = Container
   { container_location        :: Maybe Location
   , container_owner           :: Maybe EntityId
   , container_processOnUpdate :: [EntityAction]
   , container_content         :: [EntityId]
   , container_contentVolume   :: Volume
   , container_containerType   :: ContainerType
   } deriving (Generic)
makeFieldsCustom ''Container
instance HasItemType Container ItemType where itemType = containerType.itemType

instance Default Container
instance GetZIndex Container Word32 where
    get_zindex _ = toZIndex EntityZIndex_Item
