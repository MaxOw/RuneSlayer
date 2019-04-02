module Entity.TileSet
    ( module Types.Entity.TileSet

    , selectTile, toRole
    ) where

import Delude
import Engine.Common.Types
import Types.Entity.TileSet
import Types.Sprite

selectTile :: TileRole -> TileSet -> SpriteDesc
selectTile r ts = case ts^.desc of
    TileSetDesc_Custom   _ -> def
    TileSetDesc_Standard s -> selectPart s r
    where

selectPart :: SpriteDesc -> TileRole -> SpriteDesc
selectPart s = \case
    -- TileRole_Full           -> mkPart 0 5 -- (0-2,5) or (1,3) for random
    TileRole_Full           -> mkPart 1 3
    -- TileRole_Path           -> mkPart 0 0
    -- TileRole_Hole           -> mkPart 1 3
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

toRole :: ((Int, Int) -> Bool) -> [TileRole]
toRole atI = maybeToList . quadToRole $ V2
    (V2 (atI (0, 1)) (atI (1, 1)))
    (V2 (atI (0, 0)) (atI (1, 0)))

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

