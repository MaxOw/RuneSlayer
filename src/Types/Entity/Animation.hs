{-# Language TemplateHaskell #-}
module Types.Entity.Animation where

import Delude
import Resource (Resource)
import Types.Entity.Common

--------------------------------------------------------------------------------

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
   deriving (Show, Eq, Enum, Bounded)

--------------------------------------------------------------------------------

data Animation = Animation
   { animation_aniMap      :: AnimationState -> Resource -> Resource
   , animation_current     :: AnimationState
   , animation_progression :: AnimationProgression
   , animation_speed       :: Float
   }
instance Default Animation where
    def = Animation
        { animation_aniMap      = \_ -> id
        , animation_current     = def
        , animation_progression = Stopped
        , animation_speed       = 1
        }

data AnimationState = AnimationState
   { animationState_direction :: AnimationDirection
   , animationState_kind      :: AnimationKind
   , animationState_era       :: Float
   }
instance Default AnimationState where
    def = AnimationState
        { animationState_direction = South
        , animationState_kind      = Walk
        , animationState_era       = 0
        }

data AnimationProgression
   = Stopped
   | Cycle
   | TransitionInto AnimationKind AnimationProgression
   deriving (Eq)

data EffectKind
   = HitEffect AttackPower

data EffectState = EffectState
   { effectState_effectUpdate :: Time -> EffectState -> Maybe EffectState
   , effectState_kind         :: EffectKind
   , effectState_duration     :: Float
   , effectState_era          :: Float
   }

makeFieldsCustom ''Animation
makeFieldsCustom ''AnimationState
makeFieldsCustom ''EffectState
