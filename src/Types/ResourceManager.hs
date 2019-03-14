{-# Language TemplateHaskell #-}
module Types.ResourceManager where

import Delude
import Engine (Img)
import Engine.Common.Types (Size, Rect)
import Data.Aeson

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
   } deriving (Generic)
instance Default Resources

newtype SpriteName = SpriteName { unSpriteName :: Text }
    deriving (Generic, Eq, Hashable, ToJSON, FromJSON, Show)

data SpriteDesc = SpriteDesc
   { spriteDesc_name          :: SpriteName
   , spriteDesc_path          :: Text
   , spriteDesc_part          :: Maybe (Rect Int)
   , spriteDesc_pixelsPerUnit :: Maybe Int
   } deriving (Generic, Show)
makeFieldsCustom ''SpriteDesc
instance Default SpriteDesc where
    def = SpriteDesc
        { spriteDesc_name          = SpriteName ""
        , spriteDesc_path          = def
        , spriteDesc_part          = Nothing
        , spriteDesc_pixelsPerUnit = Nothing
        }

instance ToJSON SpriteDesc where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON SpriteDesc where
    parseJSON = genericParseJSON customOptionsJSON

makeFieldsCustom ''Resources
