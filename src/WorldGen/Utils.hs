module WorldGen.Utils where

import Delude

import Data.Array.Repa
-- import Data.Array.Repa.Repr.Vector (V)
-- import Data.Array.Repa.Repr.Unboxed (Unbox)
import qualified Data.Array.Repa.Algorithms.Randomish as Repa
import qualified Data.Array.Repa as Repa
-- import qualified Data.Array.Repa.Specialised.Dim2 as Repa
import qualified Data.Array.Repa.Repr.ForeignPtr as Repa
import qualified Data.Vector.Storable as F

-- import Engine.Graphics.Utils (TextureBuffer, createTextureBufferFrom)
import Codec.Picture (DynamicImage(..), Image(..))

import qualified Math.Noise as Noise
import Math.Noise (NoiseClass)
import qualified Color

--------------------------------------------------------------------------------

type RepaD2  a = Array D DIM2 a
type RepaImg   = Array D DIM3 Word8
type RepaBool  = Array D DIM2 Bool
-- type RepaFun a = DIM2 -> a

boolToImg :: Color -> Color -> RepaBool -> RepaImg
boolToImg trueColor falseColor s = Repa.traverse s e f
    where
    e (Z :. w :. h) = (Z :. w :. h :. 4)
    f atI (Z :. w :. h :. c)
        | atI (Z :. w :. h) = toC c (Color.toSRGB24 trueColor)
        | otherwise         = toC c (Color.toSRGB24 falseColor)

    toC c (Color.RGB r g b) = case c of
        0 -> r
        1 -> g
        2 -> b
        _ -> maxBound

colorImg :: Color -> RepaBool -> RepaImg -> RepaImg
colorImg trueColor bm im = Repa.traverse2 bm im e f
    where
    e _ bs = bs

    Color.RGB r g b = Color.toSRGB24 trueColor
    toC c = case c of
        0 -> r
        1 -> g
        2 -> b
        _ -> maxBound
    f atB atI i@(Z :. x :. y :. c)
        | atB (Z :. x :. y) = toC c
        | otherwise         = atI i

imgToImage :: RepaImg -> DynamicImage
imgToImage r = di
    where
    di = ImageRGBA8
       $ Image w h (F.unsafeFromForeignPtr0 (Repa.toForeignPtr rr) (h*w*z))
    (Z :. w :. h :. z) = Repa.extent r
    rr = Repa.computeS r

shrinkBool :: RepaBool -> RepaBool
shrinkBool s = Repa.traverse s id f
    where
    (Z :. w :. h) = Repa.extent s
    f atI (Z :. x :. y) = and (atIC <$> [-1..1] <*> [-1..1])
        where
        atIC a b = atI (Z :. max 0 (min w $ x+a) :. max 0 (min h $ y+b))

--------------------------------------------------------------------------------

boolCircle :: Float -> V2 Float -> DIM2 -> RepaBool
boolCircle r offV sh = Repa.map (<r) $ distanceFromPoint offV sh

distanceFromPoint :: V2 Float -> DIM2 -> RepaD2 Float
distanceFromPoint offV sh@(Z :. w :. h) = Repa.fromFunction sh f
    where
    f (Z :. x :. y) = norm cv
        where
        cv = V2 cx cy + offV
        cx = fromIntegral $ x - div w 2
        cy = fromIntegral $ y - div h 2

rndBool :: Int -> Int -> DIM2 -> RepaBool
rndBool seed probDen sh
    = Repa.map (==0)
    $ Repa.randomishIntArray sh 0 probDen seed

rndBoolCircle
    :: Int      -- Seed
    -> Int      -- Probability
    -> Float    -- Radius
    -> V2 Float -- Position
    -> DIM2     -- Array size
    -> RepaBool
rndBoolCircle _seed _probDen r offV sh
    = Repa.zipWith (&&)
    (boolCircle r offV sh)
    -- (rndBool seed probDen sh)
    (boolNoise Noise.perlin sh)

simplePerlinNoiseBool :: DIM2 -> RepaD2 Bool
simplePerlinNoiseBool = boolNoise Noise.perlin

simplePerlinNoise :: Int -> DIM2 -> RepaD2 Float
simplePerlinNoise seed = repaNoise
    $ Noise.perlin { Noise.perlinSeed = seed }

boolNoise :: NoiseClass c => c -> DIM2 -> RepaD2 Bool
boolNoise c = Repa.map (<0.05) . repaNoise c

circleCutoff :: DIM2 -> RepaD2 Float -> RepaD2 Bool
circleCutoff sh d = Repa.zipWith (\a b -> a*(max 0 (1-b)) < (0.04)) d
    -- $ Repa.map (\x->x*rs*0.5) $ distanceFromPoint 0 sh
    $ Repa.map (\x->x*rs*2) $ distanceFromPoint 0 sh
    where
    rs = (1/) $ fromIntegral $ min w h
    (Z :. w :. h) = sh

repaNoise :: NoiseClass c => c -> DIM2 -> RepaD2 Float
repaNoise c sh = Repa.fromFunction sh f
    where
    s = 1/246
    -- (Z :. iw :. ih) = sh
    -- (w, h) = (fromIntegral iw, fromIntegral ih)
    f (Z :. x :. y) = realToFrac $ fromMaybe 0
                    $ Noise.getValue c
                    (fromIntegral x*s, fromIntegral y*s, 0)

