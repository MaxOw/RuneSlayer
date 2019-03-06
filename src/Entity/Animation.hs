module Entity.Animation
    ( AnimationDirection (..)
    , AnimationKind (..)
    , AnimationProgression (..)
    , EffectKind (..)
    , EffectState
    , direction, kind, era

    , vecToDir
    , characterAnimation
    , selectCurrent
    , defaultTransition
    , update
    , makeEffect
    , renderEffect
    ) where

import Delude
import Diagrams.Angle
import Entity.Utils
import Types.Entity.Common
import Types.Entity.Animation
import qualified Resource
import Resource (Resource)

import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color

--------------------------------------------------------------------------------

vecToDir :: V2 Float -> AnimationDirection -> AnimationDirection
vecToDir v defDir
    | v == 0 = defDir
    | angleBetween v (V2   0   1 ) < 40 @@ deg = North
    | angleBetween v (V2   0 (-1)) < 40 @@ deg = South
    | angleBetween v (V2   1   0 ) < 90 @@ deg = East
    | angleBetween v (V2 (-1)  0 ) < 90 @@ deg = West
    | otherwise = defDir

{-
makeAnimationState :: V2 Float -> AnimationKind -> AnimationState
makeAnimationState v k = def
   & direction .~ vecToDir v South
   & kind      .~ k
-}

kindFrameCount :: Num a => AnimationKind -> a
kindFrameCount = \case
    Cast      -> 7
    Thrust    -> 8
    Walk      -> 9
    Slash     -> 6
    Fire      -> 11
    Die       -> 6

eraToFrame :: (HasKind a AnimationKind, HasEra a Float) => a -> Int
eraToFrame a = floor $ (kindFrameCount $ a^.kind) * (a^.era)

characterAnimation :: Animation
characterAnimation = def & aniMap .~ f
    where
    f s = \r -> Resource.mkAtlasPart r (eraToFrame s) dk
        where dk = min 20 $ fromEnum (s^.kind) * 4 + fromEnum (s^.direction)

selectCurrent :: Animation -> Resource -> Resource
selectCurrent a = (a^.aniMap) (a^.current)

defaultTransition :: AnimationProgression
defaultTransition = TransitionInto Walk Stopped

update :: Time -> Animation -> Animation
update (Time delta) a
    | newEra >= 1 = progressKind (a^.progression) $ a & current.era .~ newEra-1
    | otherwise   = a & current.era .~ newEra
    where
    d = a^.current.direction
    dirSpeed = if d == North || d == South then 1.6 else 1.2
    eraChange = a^.speed * delta * dirSpeed
    newEra
        | a^.progression == Stopped = 0
        | otherwise = a^.current.era + eraChange
    progressKind = \case
        Stopped -> id
        Cycle   -> id
        TransitionInto k p -> set (current.kind) k . set progression p

type EffectUpdate = Time -> EffectState -> Maybe EffectState
makeEffect :: EffectKind -> EffectUpdate -> EffectState
makeEffect k f = EffectState
   { effectState_effectUpdate = f
   , effectState_kind         = k
   , effectState_duration     = 1
   , effectState_era          = 0
   }

renderEffect :: EffectState -> RenderAction
renderEffect e = case e^.kind of
    HitEffect _ -> translateY (e^.era) $ renderCircle 0.1 Color.red
    where
    renderCircle s c = scale s $ renderShape $ def
        & shapeType .~ SimpleCircle
        & color     .~ Color.opaque c

