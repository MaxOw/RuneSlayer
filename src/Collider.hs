{-# Language PatternSynonyms #-}
module Collider
    ( ShapeDesc, Shape
    , constructShape
    , locateShape
    , separateShapes
    , renderShape
    , verifyConvex
    ) where

import Delude
import Data.List (zipWith3)
import Diagrams.Angle (normalizeAngle, halfTurn)
import Diagrams.TwoD.Vector (signedAngleBetween)
import Types.Entity.Common
import Types.Collider

import qualified Engine
import Engine (RenderAction, color, zindex, SimpleShape(..), shapeType)
import Engine.Common.Types
import qualified Diagrams.TwoD.Transform as T
import qualified Color

locateShape :: Location -> Shape -> Shape
locateShape (Location loc) (Shape ps bb) = Shape
    { field_parts = fmap (overBaseShapeVec (+loc)) ps
    , field_bbox  = over minPoint (+loc) . over maxPoint (+loc) $ bb
    }

separateCircleCircle :: V2 Float -> Float -> V2 Float -> Float -> Maybe (V2 Float)
separateCircleCircle va ra vb rb
    | rs >= 0   = Nothing
    | otherwise = Just $ nv ^* rs
    where
    v  = va - vb
    rs = norm v - (ra + rb)
    nv = normalize v

separateBaseShapes :: BaseShape -> BaseShape -> Maybe (V2 Float)
separateBaseShapes a b = case (a, b) of
    (BS_Circle va ra, BS_Circle vb rb) -> separateCircleCircle va ra vb rb

separateShapes :: Shape -> Shape -> Maybe (V2 Float)
separateShapes (Shape as ab) (Shape bs bb)
    | bboxIntersects ab bb = viaNonEmpty head ss
    | otherwise = Nothing
    where
    ss = catMaybes $ toList $ separateBaseShapes <$> as <*> bs
    -- case ss of
    -- [] -> Nothing
    -- _s -> Just $ vv / sl
    -- sl = fromIntegral $ length ss
    -- vv = foldl' (+) 0 ss

verifyConvex :: [V2 Float] -> Maybe ConvexPoly
verifyConvex ls
    | ll >= 3 && ss == ll = Just $ ConvexPoly ls
    | otherwise           = Nothing
    where
    ll = length ls
    calcAng a b c
        = ceiling $ signum $ view turn $ (^-^ halfTurn)
        $ normalizeAngle $ signedAngleBetween (b - a) (b - c)
    as = zipWith3 calcAng (cycle ls) (drop 1 $ cycle ls) (drop 2 $ cycle ls)
    ss = abs $ sum $ take ll as

--------------------------------------------------------------------------------

renderShape :: Shape -> RenderAction
renderShape = Engine.renderComposition . map renderBaseShape . toList . view (ff#parts)

renderBaseShape :: BaseShape -> RenderAction
renderBaseShape = \case
    BS_Circle p r -> Engine.renderShape $ def
        & shapeType .~ SimpleCircle
        & color     .~ Color.withOpacity Color.red 0.3
        & zindex    .~ 10000
        & T.scale     r
        & T.translate p

