module Entity.Timer where

import Delude
import Types.Entity.Common (Duration)
import Types.Entity.Timer
import qualified Data.DefMap as DefMap

--------------------------------------------------------------------------------

start :: TimerType -> Duration -> Timer -> Timer
start tt d (Timer t) = Timer $ DefMap.insert tt d t

update :: Duration -> Timer -> Timer
update d (Timer t) = Timer $ DefMap.mapMaybe f t
    where f v = let dd = v - d in if dd <= 0 then Nothing else Just dd

lookup :: TimerType -> Timer -> Duration
lookup tt (Timer t) = DefMap.lookup tt t
