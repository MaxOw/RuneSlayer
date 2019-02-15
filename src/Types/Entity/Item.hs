{-# Language TemplateHaskell #-}
module Types.Entity.Item where

import Delude

import Types.EntityAction
import Types.Entity.Common
import Types.Entity.ItemType
import Types.Entity.ZIndex

--------------------------------------------------------------------------------

data Item = Item
   { item_location        :: Maybe Location
   , item_owner           :: Maybe EntityId
   , item_itemType        :: ItemType
   , item_processOnUpdate :: [EntityAction]
   } deriving (Generic)
makeFieldsCustom ''Item

instance Default Item
instance GetZIndex Item Word32 where get_zindex _ = toZIndex EntityZIndex_Item
