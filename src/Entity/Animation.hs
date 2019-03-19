module Entity.Animation
    ( AnimationDirection (..)
    , AnimationKind (..)
    , AnimationProgression (..)
    , EffectKind (..)
    , EffectState
    , direction, kind, era

    , makeAnimation

    , vecToDir
    , characterAnimation
    , selectCurrent
    , defaultTransition
    , update
    , makeEffect
    , renderEffect
    ) where

import Delude
import qualified Data.Map as Map
import Diagrams.Angle
import Entity.Utils
-- import Types.Entity.Common
import Types.Sprite
import Types.Entity.Animation

import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color

--------------------------------------------------------------------------------

-- type RenderSprite = SpriteDesc -> RenderAction
makeAnimation :: AnimationDesc -> Animation
makeAnimation = \case
   CustomAnimation    ps -> makeCustomAnimation    ps
   CharacterAnimation fp -> makeCharacterAnimation fp

makeCustomAnimation :: [AnimationPart] -> Animation
makeCustomAnimation ps = def
    & set aniMap      amap
    & set progression Cycle
    where
    pmap :: Map (Maybe AnimationKind, Maybe AnimationDirection) [AnimationFrame]
    pmap = Map.fromList $ map (\p -> ((p^.kind, p^.direction), p^.frames)) ps

    selectFrames :: AnimationKind -> AnimationDirection -> [AnimationFrame]
    selectFrames k d = fromMaybe [] $ listToMaybe $ catMaybes
        [ Map.lookup (Just  k, Just  d) pmap
        , Map.lookup (Just  k, Nothing) pmap
        , Map.lookup (Nothing, Just  d) pmap
        , Map.lookup (Nothing, Nothing) pmap
        ]

    selectFrame :: Float -> [AnimationFrame] -> Maybe SpriteDesc
    selectFrame t = select t 0 . normalizeFrames

    select _ _ []     = Nothing
    select t d (f:fs) = let d' = d + f^.duration in if t < d'
        then Just (f^.sprite)
        else select t d' fs

    normalizeFrames fs = map (over duration (/n)) fs
        where n = genericLength fs

    amap s = selectFrame (s^.era) $ selectFrames (s^.kind) (s^.direction)

makeCharacterAnimation :: FilePath -> Animation
makeCharacterAnimation _fp = def
    where

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
    -- TODO
    f s = Nothing
    -- f s = \r -> Resource.mkAtlasPart r (eraToFrame s) dk
        -- where dk = min 20 $ fromEnum (s^.kind) * 4 + fromEnum (s^.direction)

selectCurrent :: Animation -> Maybe SpriteDesc
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
    HitEffect x -> translateY (e^.era) $ renderHit x
    where
    renderHit (AttackPower x) = scale (1/64) $ renderSimpleText d $ show (-x)
    d = def & color .~ Color.opaque Color.red
    {-
    renderCircle s c = scale s $ renderShape $ def
        & shapeType .~ SimpleCircle
        & color     .~ Color.opaque c
    -}

