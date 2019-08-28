module Data.Timer where

import Delude
import Types.Entity.Common (Duration)
import qualified Data.Map as Map

--------------------------------------------------------------------------------

newtype Timer a = Timer (Map a Duration)
    deriving (Generic, Default)

--------------------------------------------------------------------------------

start :: Ord a => a -> Duration -> Timer a -> Timer a
start tt d (Timer t) = Timer $ Map.insert tt d t

update :: Duration -> Timer a -> Timer a
update d (Timer t) = Timer $ Map.mapMaybe f t
    where f v = let dd = v - d in if dd <= 0 then Nothing else Just dd

lookup :: Ord a => a -> Timer a -> Duration
lookup tt (Timer t) = fromMaybe def $ Map.lookup tt t

isTimerUp :: Ord a => a -> Timer a -> Bool
isTimerUp tt = (<=0) . lookup tt
