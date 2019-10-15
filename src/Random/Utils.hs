module Random.Utils where

import Delude
import Entity.Utils
import Diagrams.Prelude (turn, unitX)

import Control.Monad.ST
-- import System.Random (StdGen, mkStdGen)
import qualified System.Random.MWC as MWC
import qualified Data.Vector.Unboxed as Vector
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Primitive

--------------------------------------------------------------------------------

-- type RandomST a = forall s. MWC.GenST s -> ST s a

type RandomP m a = ReaderT (MWC.Gen (PrimState m)) m a
type RandomIO  a = ReaderT MWC.GenIO IO a
type RandomST  a = forall s. ReaderT (MWC.GenST s) (ST s) a

-- This is probably not thread-safe...
{-# NoInline genIO #-}
genIO :: MWC.GenIO
genIO = unsafePerformIO MWC.createSystemRandom

-- liftRandomST :: (forall s. MWC.GenST s -> ST s a) -> RandomST a
-- liftRandomST = ReaderT

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
    runReaderT (do {_ <- uniform @Int; f})
    =<< MWC.initialize (Vector.fromList s)

runRandom :: Integral i => i -> RandomST a -> a
runRandom s = randomFromSeed [fromIntegral s]

runRandomIO :: MonadIO m => RandomIO a -> m a
runRandomIO r = liftIO $ runReaderT r genIO

uniform :: (MWC.Variate a, PrimMonad m) => RandomP m a
uniform = ReaderT MWC.uniform

uniformRange :: (MWC.Variate a, PrimMonad m) => (a,a) -> RandomP m a
uniformRange r = ReaderT $ \gen -> MWC.uniformR r gen

-- This is an inefficient O(n) weighted selection for one-off useage.
-- If you need something more efficient (for multiple selection situations) use
-- CondensedTable functionality from mwc-random package.
uniformSelectWeighted :: forall a m. PrimMonad m
    => [(Float, a)] -> RandomP m (Maybe a)
uniformSelectWeighted = sel . filter ((>0) . fst)
    where
    sel []       = return Nothing
    sel [(_, a)] = return (Just a)
    sel ls = do
        let ss = sumOf (traverse._1) ls
        r <- uniformRange (0, ss)
        return $ go 0 r ls
        where
        go _ _ [] = Nothing
        go _ _ [(_, a)] = Just a
        go c r ((p,a):as)
            | r < c+p   = Just a
            | otherwise = go (c+p) r as

randomDirection :: PrimMonad m => RandomP m V2D
randomDirection = do
    r <- ReaderT MWC.uniform
    return $ rotate (r @@ turn) unitX

randomListSelect :: forall a m. PrimMonad m => [a] -> RandomP m (Maybe a)
randomListSelect ls = do
    let l = length ls
    r <- uniformRange (0, l-1)
    return $ viaNonEmpty head $ drop r ls

