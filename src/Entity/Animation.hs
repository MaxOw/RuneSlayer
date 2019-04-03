module Entity.Animation
    ( Direction (..)
    , AnimationKind (..)
    , AnimationProgression (..)
    , EffectKind (..)
    , EffectState
    , direction, kind, era

    , makeAnimation
    , renderAnimation

    , vecToDir
    , defaultTransition
    , update
    , makeEffect
    , renderEffect
    ) where

import Delude
import qualified Data.Map as Map
import Engine.Common.Types (Rect (..))
import Diagrams.Angle
import qualified Diagrams.TwoD.Transform as T
import Entity.Utils
import Types.Sprite
import Types.Entity.Animation

import ResourceManager (Resources, renderSprite)

import qualified Color

--------------------------------------------------------------------------------

renderAnimation :: AnimationState -> Animation -> RenderAction
renderAnimation s a = runAnimation a (s^.current)

makeAnimation :: Resources -> AnimationDesc -> Animation
makeAnimation rs desc = case desc of
   CustomAnimation    ps -> makeCustomAnimation    rs ps
   CharacterAnimation sd -> makeCharacterAnimation rs sd

makeCustomAnimation
    :: Resources -> [AnimationPart] -> Animation
makeCustomAnimation rs ps = Animation $ maybe mempty (renderSprite rs) . amap
    where
    pmap :: Map (Maybe AnimationKind, Maybe Direction) [AnimationFrame]
    pmap = Map.fromList $ map (\p -> ((p^.kind, p^.direction), p^.frames)) ps

    selectFrames :: AnimationKind -> Direction -> [AnimationFrame]
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

makeCharacterAnimation :: Resources -> SpriteDesc -> Animation
makeCharacterAnimation rs sd = Animation $ \s ->
    renderSprite rs (selectFrame s sd)
    where
    selectFrame :: AnimationFrameState -> SpriteDesc -> SpriteDesc
    selectFrame s = set part (Just $ framePart s)

    framePart s = makePart (eraToFrame s) dk
        where dk = min 20 $ fromEnum (s^.kind) * 4 + fromEnum (s^.direction)

    makePart x y = Rect (ss *^ V2 x y) (pure ss)
        where ss = 64

vecToDir :: V2 Float -> Direction -> Direction
vecToDir v defDir
    | v == 0 = defDir
    | angleBetween v (V2   0   1 ) < 40 @@ deg = North
    | angleBetween v (V2   0 (-1)) < 40 @@ deg = South
    | angleBetween v (V2   1   0 ) < 90 @@ deg = East
    | angleBetween v (V2 (-1)  0 ) < 90 @@ deg = West
    | otherwise = defDir

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

defaultTransition :: AnimationProgression
defaultTransition = TransitionInto Walk Stopped

update :: Time -> AnimationState -> AnimationState
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
   { field_effectUpdate = f
   , field_kind         = k
   , field_duration     = 1
   , field_era          = 0
   }

renderEffect :: EffectState -> RenderAction
renderEffect e = case e^.kind of
    HitEffect x -> translateY (e^.era) $ renderHit x
    where
    renderHit (AttackPower x) = T.scale (1/64) $ renderSimpleText d $ show (-x)
    d = def & color .~ Color.opaque Color.red
