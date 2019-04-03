module Types.Entity.Item where

import Delude

import Types.EntityAction
import Types.Entity.Common
import Types.Entity.ItemType
import Types.Entity.ZIndex
import Types.Entity.Animation (Direction)

--------------------------------------------------------------------------------

data Item = Item
   { field_location        :: Maybe Location
   , field_owner           :: Maybe EntityId
   , field_itemType        :: ItemType
   , field_processOnUpdate :: [EntityAction]
   , field_content         :: [EntityId]
   , field_contentVolume   :: Volume
   , field_direction     :: Maybe Direction
   } deriving (Generic)


instance Default Item
instance GetZIndex Item Word32 where get_zindex _ = toZIndex EntityZIndex_Item
