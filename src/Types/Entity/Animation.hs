{-# Language TemplateHaskell #-}
module Types.Entity.Animation where

import Delude

data AnimationDirection
   = North
   | West
   | South
   | East
   deriving (Eq, Enum, Bounded)

data AnimationKind
   = Cast
   | Thrust
   | Walk
   | Slash
   | Fire
   | Die
   deriving (Show, Enum, Bounded)

data CharacterAnimation = CharacterAnimation
   { characterAnimation_direction :: AnimationDirection
   , characterAnimation_kind      :: AnimationKind
   , characterAnimation_era       :: Float
   }
makeFieldsCustom ''CharacterAnimation
instance Default CharacterAnimation where
    def = CharacterAnimation
        { characterAnimation_direction = South
        , characterAnimation_kind      = Walk
        , characterAnimation_era       = 0
        }
