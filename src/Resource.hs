module Resource (module Resource) where

import Delude
import Types.ResourceManager as Resource (Resource(..))
import Types.ResourceManager
import Engine.Common.Types

allSprites :: [Resource]
allSprites =
    [ healthPotion
    , atlasItems1
    , atlasEnv1
    ]

mkResourcePng :: Text -> Resource
mkResourcePng p = def
    & path .~ "data/imgs/" <> p <> ".png"

{-
mkResourcePngPart :: Int -> Text -> Int -> Int -> Resource
mkResourcePngPart s p x y = def
    & path .~ "data/imgs/" <> p <> ".png"
    & part .~ mkRectOf s s x y
-}

mkAtlasPart :: Resource -> Int -> Int -> Resource
mkAtlasPart atl x y = atl
    & part .~ Just (mkRectOf (atl^.gridSize) (Rect (V2 x y) (pure 1)))

mkAtlasRect :: Resource -> Int -> Int -> Int -> Int -> Resource
mkAtlasRect atl x y w h = atl
    & part .~ Just (mkRectOf (atl^.gridSize) (Rect (V2 x y) (Size w h)))

mkRectOf :: Size Int -> Rect Int -> Rect Int
mkRectOf (Size w h) (Rect (V2 x y) (Size ww hh)) = Rect (V2 fx fy) (Size fw fh)
    where
    fx = x * w
    fy = y * h
    fw = ww * w
    fh = hh * h
    -- sw = 1 / fromIntegral w
    -- sh = 1 / fromIntegral h

healthPotion :: Resource
healthPotion = mkResourcePng "items0/P_Red07"

atlasItems1 :: Resource
atlasItems1 = mkResourcePng "items1/items1"
    -- & gridSize .~ pure 16
    & gridSize .~ pure 32

atlasEnv1 :: Resource
atlasEnv1 = mkResourcePng "environment/base_out_atlas"
    -- & gridSize .~ pure 64
    & gridSize .~ pure 16

helmet :: Resource
helmet = mkAtlasPart atlasItems1 1 1

bag :: Resource
bag = mkAtlasPart atlasItems1 4 2

mkEnvPart :: Int -> Int -> Resource
mkEnvPart = mkAtlasPart atlasEnv1

mkEnvRect :: Int -> Int -> Int -> Int -> Resource
mkEnvRect = mkAtlasRect atlasEnv1
