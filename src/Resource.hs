module Resource (module Resource) where

import Delude
import Types.ResourceManager as Resource (Resource(..))
import Types.ResourceManager
import Engine.Common.Types

mkResourcePng :: Float -> Text -> Resource
mkResourcePng ppu p = def
    & pixelsPerUnit .~ ppu
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

bat :: Resource
bat = mkResourcePng 32 "units/bat"
    & gridSize .~ pure 32

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

atlasEnv1 :: Resource
atlasEnv1 = mkResourcePng 32 "environment/base_out_atlas"
    -- & gridSize .~ pure 64
    & gridSize .~ pure 16

mkEnvRect :: Int -> Int -> Int -> Int -> Resource
mkEnvRect = mkAtlasRect atlasEnv1

