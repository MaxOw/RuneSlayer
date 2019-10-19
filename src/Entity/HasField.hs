{-# Language CPP #-}
{-# Language MonoLocalBinds #-}
{-# Language DefaultSignatures #-}
module Entity.HasField (module Entity.HasField) where

import Control.Lens
import HasField

#define MakeFieldLens(X) X :: HasF "X" s a => Lens' s a; X = ff#X

class HasStandingWeight s a | s -> a where
    standingWeight :: Lens' s a
    default standingWeight :: HasF "standingWeight" s a => Lens' s a
    standingWeight = ff#standingWeight

class HasCollisionBits s a | s -> a where
    collisionBits :: Lens' s a
    default collisionBits :: HasF "collisionBits" s a => Lens' s a
    collisionBits = ff#collisionBits

MakeFieldLens(stats)
MakeFieldLens(interactions)
MakeFieldLens(primaryInteraction)
MakeFieldLens(labelOffset)
MakeFieldLens(canAttackTarget)
