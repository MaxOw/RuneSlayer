module Random.Utils where

import Delude
import Entity.Utils
import Diagrams.Prelude (turn, unitX)

import Control.Monad.ST
-- import System.Random (StdGen, mkStdGen)
import qualified System.Random.MWC as MWC
import qualified Data.Vector.Unboxed as Vector

--------------------------------------------------------------------------------

-- type RandomST a = forall s. MWC.GenST s -> ST s a

type RandomST a = forall s. ReaderT (MWC.GenST s) (ST s) a

liftRandom :: (forall s. MWC.GenST s -> ST s a) -> RandomST a
liftRandom = ReaderT

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
randomFromSeed s f = runST $ do
    runReaderT (do {_ <- uniform :: RandomST Int; f})
    =<< MWC.initialize (Vector.fromList s)

runRandom :: Int -> RandomST a -> a
runRandom s = randomFromSeed [fromIntegral s]

uniform :: MWC.Variate a => RandomST a
uniform = ReaderT MWC.uniform

uniformRange :: MWC.Variate a => (a,a) -> RandomST a
uniformRange r = ReaderT $ \gen -> MWC.uniformR r gen

randomDirection :: RandomST V2D
randomDirection = do
    r <- ReaderT MWC.uniform
    return $ rotate (r @@ turn) unitX

randomListSelect :: [a] -> RandomST (Maybe a)
randomListSelect ls = do
    let l = length ls
    r <- uniformRange (0, l-1)
    return $ viaNonEmpty head $ drop r ls

