module Types.Entity.Appearance where

import Delude

import Types.ResourceManager (Resource)
import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color

data Appearance
   = Appearance_SimpleCircle Float AlphaColor
   | Appearance_SimpleSquare Float AlphaColor
   | Appearance_Sprite       Resource
   | Appearance_Translate (V2 Float) Appearance
   | Appearance_Compose [Appearance]
instance Default Appearance where
    def = Appearance_SimpleCircle 1 $ Color.opaque Color.red

