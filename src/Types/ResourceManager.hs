{-# Language TemplateHaskell #-}
module Types.ResourceManager (module Types.ResourceManager) where

import Delude
import Engine (Img)
import Engine.Common.Types (Size, Rect)
import Types.Sprite as Types.ResourceManager
import Types.Entity.ItemType

data Resource = Resource
   { resource_path          :: Text
   , resource_part          :: Maybe (Rect Int)
   , resource_gridSize      :: Size Int
   , resource_pixelsPerUnit :: Float
   }
   deriving (Show, Generic)
-- instance Hashable Resource
makeFieldsCustom ''Resource
instance Default Resource where
    def = Resource
        { resource_path          = def
        , resource_part          = Nothing
        , resource_gridSize      = pure 1
        , resource_pixelsPerUnit = 1
        }
-- instance ToJSON Resource where
    -- toEncoding = genericToEncoding defaultOptions


data Resources = Resources
   { resources_resourceMap :: HashMap Text Img
   , resources_spriteMap   :: HashMap SpriteName SpriteDesc
   , resources_itemsMap    :: HashMap ItemTypeName ItemType
   } deriving (Generic)
instance Default Resources

makeFieldsCustom ''Resources
