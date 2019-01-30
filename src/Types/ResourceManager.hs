{-# Language TemplateHaskell #-}
module Types.ResourceManager where

import Delude
import Engine (Img)
import Engine.Common.Types (Rect, unitRect)

data Resource = Resource
   { resource_path :: Text
   , resource_part :: Rect Float
   }
   deriving (Show, Generic)
-- instance Hashable Resource
makeFieldsCustom ''Resource
instance Default Resource where
    def= Resource
        { resource_path = def
        , resource_part = unitRect
        }

type ResourceMap = HashMap Text Img
