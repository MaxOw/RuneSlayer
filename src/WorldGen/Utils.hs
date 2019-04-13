module WorldGen.Utils where

import Delude

import Data.Array.Repa
-- import Data.Array.Repa.Repr.Vector (V)
-- import Data.Array.Repa.Repr.Unboxed (Unbox)
-- import qualified Data.Array.Repa.Algorithms.Randomish as Repa
import qualified Data.Array.Repa as Repa
-- import qualified Data.Array.Repa.Specialised.Dim2 as Repa
import qualified Data.Array.Repa.Repr.ForeignPtr as Repa
import qualified Data.Vector.Storable as F

-- import Engine.Graphics.Utils (TextureBuffer, createTextureBufferFrom)
import Codec.Picture (DynamicImage(..), Image(..))

--------------------------------------------------------------------------------

type RepaImg   = Array D DIM3 Word8
type RepaBool  = Array D DIM2 Bool
type RepaFun a = DIM2 -> a

boolToImg :: RepaBool -> RepaImg
boolToImg s = Repa.traverse s e f
    where
    e (Z :. w :. h) = (Z :. w :. h :. 4)
    f atI (Z :. w :. h :. c)
        | c == 3            = maxBound
        | atI (Z :. w :. h) = maxBound
        | otherwise         = minBound

-- imgToImage :: MonadIO m => RepaImg -> m DynamicImage
imgToImage :: RepaImg -> DynamicImage
imgToImage r = di
    where
    di = ImageRGBA8
       $ Image w h (F.unsafeFromForeignPtr0 (Repa.toForeignPtr rr) (h*w*z))
    (Z :. w :. h :. z) = Repa.extent r
    rr = Repa.computeS r

--------------------------------------------------------------------------------

boolCircle :: Float -> V2 Float -> DIM2 -> RepaFun Bool
boolCircle r offV (Z :. w :. h) = f
    where
    f (Z :. x :. y) = norm cv > r
        where
        cv = V2 cx cy + offV
        cx = fromIntegral $ x - div w 2
        cy = fromIntegral $ y - div h 2
