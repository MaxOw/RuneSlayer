module Random
    ( Seed
    ) where

import Delude
import qualified Data.Vector as Vector
import Control.Monad.ST
import System.Random.MWC (Variate, GenST)
import qualified System.Random.MWC as MWC

newtype Seed = Seed { unSeed :: Word32 }
type Random a = forall s. GenST s -> ST s a

withSeed :: Seed -> Random a -> (Seed, a)
withSeed s rnd = runST $ do
    gen <- MWC.restore $ MWC.toSeed $ Vector.fromList [unSeed s]
    a <- rnd gen
    newSeed <- MWC.uniform gen
    return (Seed newSeed, a)
