module Types.Entity.Appearance where

import Delude

import Types.ResourceManager (Resource)
import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color

data Appearance
   = Appearance_SimpleCircle Double AlphaColor
   | Appearance_SimpleSquare Double AlphaColor
   | Appearance_Sprite       Resource
   | Appearance_Translate (V2 Double) Appearance
   | Appearance_Compose [Appearance]
instance Default Appearance where
    def = Appearance_SimpleCircle 1 $ Color.opaque Color.red

