{-# Language TemplateHaskell #-}
module Types.ResourceManager where

import Delude
import Engine (Img)
import Engine.Common.Types (Size, Rect)

data Resource = Resource
   { resource_path     :: Text
   , resource_part     :: Maybe (Rect Int)
   , resource_gridSize :: Size Int
   }
   deriving (Show, Generic)
-- instance Hashable Resource
makeFieldsCustom ''Resource
instance Default Resource where
    def = Resource
        { resource_path     = def
        , resource_part     = Nothing
        , resource_gridSize = pure 1
        }

type ResourceMap = HashMap Text Img
