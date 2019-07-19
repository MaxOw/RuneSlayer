{-# Language CPP #-}
{-# Language MonoLocalBinds #-}
module Entity.HasField (module Entity.HasField) where

import Control.Lens
import HasField

#define MakeFieldLens(X) X :: HasF "X" s a => Lens' s a; X = ff#X

MakeFieldLens(stats)
