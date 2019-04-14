module Random.Utils where

import Delude
import Entity.Utils
import Diagrams.Prelude (turn, unitX)

import Control.Monad.ST
-- import System.Random (StdGen, mkStdGen)
import qualified System.Random.MWC as MWC
import qualified Data.Vector.Unboxed as Vector

--------------------------------------------------------------------------------

type RandomST a = forall s. MWC.GenST s -> ST s a

-- type RandomST a = ReaderT (ST s

{-
pureRandomSeed :: MWC.Seed
pureRandomSeed = runST $ MWC.save =<< MWC.create

withRandomSeed
    :: MonadState x m
    => HasRandomSeed x MWC.Seed
    => RandomST a -> m a
withRandomSeed f = do
    s <- use randomSeed
    let (a, s') = runST $ MWC.restore s >>= \g -> (,) <$> f g <*> MWC.save g
    assign randomSeed s'
    return a
-}

randomFromSeed :: [Word32] -> RandomST a -> a
randomFromSeed s f = runST $ f =<< MWC.initialize (Vector.fromList s)

runRandom :: Int -> RandomST a -> a
runRandom s = randomFromSeed [fromIntegral s]

uniformRange :: MWC.Variate a => (a,a) -> RandomST a
uniformRange = MWC.uniformR

randomDirection :: RandomST V2D
randomDirection gen = do
    r <- MWC.uniform gen
    return $ rotate (r @@ turn) unitX

