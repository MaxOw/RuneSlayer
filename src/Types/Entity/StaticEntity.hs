{-# Language TemplateHaskell #-}
module Types.Entity.StaticEntity where

import Delude

import Types.Entity.Common (Location)
import Types.Entity.ZIndex
import Types.Entity.Appearance

data StaticEntityType = StaticEntityType
   { staticEntityType_name       :: Text
   , staticEntityType_appearance :: Appearance
   } deriving (Generic)
makeFieldsCustom ''StaticEntityType
instance Default StaticEntityType

data StaticEntity = StaticEntity
   { staticEntity_location   :: Location
   , staticEntity_entityType :: StaticEntityType
   } deriving (Generic)
makeFieldsCustom ''StaticEntity
instance Default StaticEntity

instance GetZIndex StaticEntity Word32 where
    get_zindex _ = toZIndex EntityZIndex_Vertical
