module Entity.Animation
    ( AnimationDirection (..)
    , AnimationKind (..)
    , CharacterAnimation
    , direction, kind, era

    , vecToDir
    , makeAnimation
    , selectPart
    ) where

import Delude
import Diagrams.Angle
import Types.Entity.Animation
import qualified Resource
import Resource (Resource)

--------------------------------------------------------------------------------

vecToDir :: V2 Float -> AnimationDirection -> AnimationDirection
vecToDir v defDir
    | v == 0 = defDir
    | angleBetween v (V2   0   1 ) < 40 @@ deg = North
    | angleBetween v (V2   0 (-1)) < 40 @@ deg = South
    | angleBetween v (V2   1   0 ) < 90 @@ deg = East
    | angleBetween v (V2 (-1)  0 ) < 90 @@ deg = West
    | otherwise = defDir

makeAnimation :: V2 Float -> AnimationKind -> CharacterAnimation
makeAnimation v k = def
   & direction .~ vecToDir v South
   & kind      .~ k

selectPart :: CharacterAnimation -> Resource -> Resource
selectPart a r = Resource.mkAtlasPart r (eraToFrame a) dk
    where
    dk = min 20 $ fromEnum (a^.kind) * 4 + fromEnum (a^.direction)

kindFrameCount :: Num a => AnimationKind -> a
kindFrameCount = \case
    Cast      -> 7
    Thrust    -> 8
    Walk      -> 9
    Slash     -> 6
    Fire      -> 11
    Die       -> 6

eraToFrame :: CharacterAnimation -> Int
eraToFrame a = floor $ (kindFrameCount $ a^.kind) * (a^.era)
