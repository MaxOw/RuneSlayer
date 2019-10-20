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
import Relude.Extra.Foldable1
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

separateProjection :: (Float, Float) -> (Float, Float) -> Maybe Float
separateProjection a b
    | fst a <= fst b = negate <$> doSep a b
    | otherwise      = doSep b a
    where
    doSep (ma, xa) (mb, xb) =
        if xa <= mb
        then Nothing
        else Just ss
        where
        ss = if abs sl < abs sr then sl else sr
        sl = mb - xa
        sr = xb - ma

separateCircleCircle :: V2 Float -> Float -> V2 Float -> Float -> Maybe (V2 Float)
separateCircleCircle va ra vb rb
    | rs >= 0   = Nothing
    | otherwise = Just $ nv ^* rs
    where
    v  = va - vb
    rs = norm v - (ra + rb)
    nv = normalize v

separateRectRect
    :: V2 Float -> Size Float
    -> V2 Float -> Size Float
    -> Maybe (V2 Float)
separateRectRect va sa vb sb = f <$> xSep <*> ySep
    where
    BBox ma xa = mkBBoxCenter va sa
    BBox mb xb = mkBBoxCenter vb sb

    f xx yy = if abs xx < abs yy then V2 xx 0 else V2 0 yy

    xSep = separateProjection (ma^._x, xa^._x) (mb^._x, xb^._x)
    ySep = separateProjection (ma^._y, xa^._y) (mb^._y, xb^._y)

separateRectCircle
    :: V2 Float -> Size Float
    -> V2 Float -> Float
    -> Maybe (V2 Float)
separateRectCircle rp s cp r = f <$> mrr <*> mpp
    where
    f a b = if norm a < norm b then a else b
    mrr = separateRectRect rp s cp (pure $ 2*r)
    ps  = rectToList $ bboxToRect $ mkBBoxCenter rp s
    mc  = normalize <$> viaNonEmpty head $ sortOn norm $ map (cp-) ps
    mpr = projectPointsRange <$> mc <*> nonEmpty ps
    mcr = projectCircleRange <$> mc
    mpp = (^*) <$> mc <*> (mpr >>= \pr -> separateProjection pr =<< mcr)

    projectCircleRange p = (dd-r, dd+r)
        where dd = dot cp p

projectPointsRange :: V2 Float -> NonEmpty (V2 Float) -> (Float, Float)
projectPointsRange p vs = (minimum1 ns, maximum1 ns)
    where
    ns = fmap (dot ?? p) vs

separateBaseShapes :: BaseShape -> BaseShape -> Maybe (V2 Float)
separateBaseShapes a b = case (a, b) of
    (BS_Circle va ra, BS_Circle vb rb) -> separateCircleCircle va ra vb rb
    (BS_Rect   va sa, BS_Rect   vb sb) -> separateRectRect     va sa vb sb
    (BS_Rect   va sa, BS_Circle vb rb) -> separateRectCircle   va sa vb rb
    (BS_Circle va ra, BS_Rect   vb sb) -> separateRectCircle   vb sb va ra <&> negate

separateShapes :: Shape -> Shape -> Maybe (V2 Float)
separateShapes (Shape as ab) (Shape bs bb)
    | bboxIntersects ab bb = viaNonEmpty head ss
    | otherwise = Nothing
    where
    ss = catMaybes $ toList $ separateBaseShapes <$> as <*> bs

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
renderShape = Engine.renderComposition . map renderBaseShape . toList . (^.ff#parts)

renderBaseShape :: BaseShape -> RenderAction
renderBaseShape = \case
    BS_Circle p r -> Engine.renderShape $ def
        & shapeType .~ SimpleCircle
        & color     .~ Color.withOpacity Color.red 0.3
        & zindex    .~ 10000
        & T.scale     r
        & T.translate p
    BS_Rect p (Size w h) -> Engine.renderShape $ def
        & shapeType .~ SimpleSquare
        & color     .~ Color.withOpacity Color.red 0.3
        & zindex    .~ 10000
        & T.scaleX    w
        & T.scaleY    h
        & T.translate p
