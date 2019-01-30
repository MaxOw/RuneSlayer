module Resource (module Resource) where

import Delude
import Types.ResourceManager as Resource (Resource(..))
import Engine.Common.Types

allSprites :: [Resource]
allSprites =
    [ healthPotion
    , helmet
    , bag
    ]

mkResourcePng :: Text -> Resource
mkResourcePng p = def
    & path .~ "data/imgs/" <> p <> ".png"

mkResourcePngPart :: Int -> Text -> Int -> Int -> Resource
mkResourcePngPart s p x y = def
    & path .~ "data/imgs/" <> p <> ".png"
    & part .~ mkRectOf s s x y

mkRectOf :: Int -> Int -> Int -> Int -> Rect Float
mkRectOf w h x y = Rect (V2 fx fy) (Size fw fh)
    where
    fx = fromIntegral x * fw
    fy = fromIntegral y * fh
    fw = 1 / fromIntegral w
    fh = 1 / fromIntegral h

healthPotion :: Resource
healthPotion = mkResourcePng "items0/P_Red07"

helmet :: Resource
helmet = mkResourcePngPart 16 "items1/items1" 1 1

bag :: Resource
bag = mkResourcePngPart 16 "items1/items1" 4 2

