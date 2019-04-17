module Entity.TileSet
    ( module Types.Entity.TileSet

    , selectTile, toRole, complem
    ) where

import Delude
import Engine.Common.Types
import Random.Utils

import Types.Entity.TileSet
import Types.Sprite

selectTile :: Int -> TileRole -> TileSet -> SpriteDesc
selectTile rndSeed r ts = case ts^.ff#desc of
    TileSetDesc_Custom   _ -> def
    TileSetDesc_Standard s -> selectPart rndSeed s r

selectPart :: Int -> SpriteDesc -> TileRole -> SpriteDesc
selectPart rndSeed s = \case
    TileRole_Full           -> mkPartSelect
    TileRole_Edge        ed -> mkEdge ed
    TileRole_OuterCorner oc -> mkOuterCorner oc
    TileRole_InnerCorner ic -> mkInnerCorner ic
    TileRole_Cross       cr -> mkCross cr
    where
    mkEdge = \case
        Edge_Top    -> mkPart 1 2
        Edge_Left   -> mkPart 0 3
        Edge_Right  -> mkPart 2 3
        Edge_Bottom -> mkPart 1 4
    mkOuterCorner = \case
        Corner_TopLeft     -> mkPart 0 2
        Corner_TopRight    -> mkPart 2 2
        Corner_BottomLeft  -> mkPart 0 4
        Corner_BottomRight -> mkPart 2 4
    mkInnerCorner = \case
        Corner_TopLeft     -> mkPart 1 0
        Corner_TopRight    -> mkPart 2 0
        Corner_BottomLeft  -> mkPart 1 1
        Corner_BottomRight -> mkPart 2 1
    mkCross = \case
        Cross_TopLeftBottomRight -> mkPart 4 1
        Cross_BottomLeftTopRight -> mkPart 4 2

    mkPart x y = set part (Just p) s
        where
        p = Rect (ss *^ (V2 x y)) (pure ss)
        ss = 32

    mkPartSelect
        = uncurry mkPart $ fromMaybe (1,2)
        $ runRandom rndSeed $ randomListSelect
        $ replicate 12 (1,3) <> replicate 2 (2,5) <> [(0,5), (1,5)]

toRole :: ((Int, Int) -> Bool) -> Maybe TileRole
toRole atI = quadToRole $ V2
    (V2 (atI (0, 1)) (atI (1, 1)))
    (V2 (atI (0, 0)) (atI (1, 0)))

complem :: TileRole -> TileRole
complem = \case
    TileRole_Full           -> TileRole_Full
    TileRole_Edge        e  -> TileRole_Edge $ compE e
    TileRole_OuterCorner oc -> TileRole_InnerCorner oc
    TileRole_InnerCorner ic -> TileRole_OuterCorner ic
    TileRole_Cross       cr -> TileRole_Cross $ compX cr
    where
    compE = \case
        Edge_Bottom -> Edge_Top
        Edge_Left   -> Edge_Right
        Edge_Top    -> Edge_Bottom
        Edge_Right  -> Edge_Left

    compX = \case
        Cross_TopLeftBottomRight -> Cross_BottomLeftTopRight
        Cross_BottomLeftTopRight -> Cross_TopLeftBottomRight

quadToRole :: M22 Bool -> Maybe TileRole
quadToRole
    (V2 (V2 a b)
        (V2 d c))
    | a = if
        | b -> if
            | c -> if
                | d -> Just TileRole_Full
                | otherwise -> Just $ TileRole_InnerCorner Corner_TopRight
            | d -> Just $ TileRole_InnerCorner Corner_TopLeft
            | otherwise -> Just $ TileRole_Edge Edge_Bottom
        | c -> if
            | d -> Just $ TileRole_InnerCorner Corner_BottomLeft
            | otherwise -> Just $ TileRole_Cross Cross_BottomLeftTopRight
        | d -> Just $ TileRole_Edge Edge_Right
        | otherwise -> Just $ TileRole_OuterCorner Corner_BottomRight
    | b = if
        | c -> if
            | d -> Just $ TileRole_InnerCorner Corner_BottomRight
            | otherwise -> Just $ TileRole_Edge Edge_Left
        | d -> Just $ TileRole_Cross Cross_TopLeftBottomRight
        | otherwise -> Just $ TileRole_OuterCorner Corner_BottomLeft
    | c = if
        | d -> Just $ TileRole_Edge Edge_Top
        | otherwise -> Just $ TileRole_OuterCorner Corner_TopLeft
    | d = Just $ TileRole_OuterCorner Corner_TopRight
    | otherwise = Nothing

