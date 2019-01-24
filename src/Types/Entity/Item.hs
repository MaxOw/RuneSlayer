{-# Language TemplateHaskell #-}
module Types.Entity.Item where

import Delude

import Types.EntityAction
import Types.Entity.Common
import Types.Entity.ItemType

--------------------------------------------------------------------------------

data Item = Item
   { item_location        :: Maybe Location
   , item_owner           :: Maybe EntityId
   , item_itemType        :: ItemType
   , item_processOnUpdate :: [EntityAction]
   } deriving (Generic)
makeFieldsCustom ''Item

instance Default Item
