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
    , maleBody
    , maleHair
    , malePants
    , maleShirt
    ]

mkResourcePng :: Float -> Text -> Resource
mkResourcePng ppu p = def
    & unitsPerPixel .~ (1/ppu)
    & path .~ "data/imgs/" <> p <> ".png"

mkAnimation :: Text -> Resource
mkAnimation t = mkResourcePng 32 ("characters/" <> t)
    -- & gridSize .~ (Size 13 21)
    & gridSize .~ pure 64

maleBody :: Resource
maleBody = mkAnimation "body/male/light"

maleHair :: Resource
maleHair = mkAnimation "hair/male/plain/brown"

malePants :: Resource
malePants = mkAnimation "legs/pants/male/teal_pants_male"

maleShirt :: Resource
maleShirt = mkAnimation "torso/shirts/longsleeve/male/white_longsleeve"


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
healthPotion = mkResourcePng 64 "items0/P_Red07"

atlasItems1 :: Resource
atlasItems1 = mkResourcePng 32 "items1/items1"
    -- & gridSize .~ pure 16
    & gridSize .~ pure 32

atlasEnv1 :: Resource
atlasEnv1 = mkResourcePng 32 "environment/base_out_atlas"
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

treeTrunk :: Resource
treeTrunk = mkEnvRect 50 38 6 6

treeFoliage :: Resource
treeFoliage = mkEnvRect 48 24 6 6
