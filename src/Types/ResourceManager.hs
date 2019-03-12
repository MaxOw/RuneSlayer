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

type ResourceMap = HashMap Text Img

instance ToJSON Resource where
    toEncoding = genericToEncoding defaultOptions
