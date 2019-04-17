module Color (module Out, ColorDesc, fromColorDesc) where

import Relude -- (Maybe (..), String, Generic)
import Data.Aeson (FromJSON)
import Data.Char (toLower)
import Data.Colour           as Out
import Data.Colour.Names     as Out
import Data.Colour.SRGB      as Out
import Engine.Graphics.Types as Out (Color, AlphaColor)

newtype ColorDesc = ColorDesc String deriving (Generic, FromJSON)

fromColorDesc :: ColorDesc -> Maybe Color
fromColorDesc (ColorDesc str) = case readColourName ss of
    Just x  -> Just x
    Nothing -> case ss of
        ('#':r:g:b:[]) -> readRGB $ "#" <> [r,r] <> [g,g] <> [b,b]
        _ -> readRGB ss
    where
    readRGB = fmap fst . listToMaybe . sRGB24reads
    ss = map toLower str

