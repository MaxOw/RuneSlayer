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
    TileRole_Path           -> mkPart 0 0
    TileRole_Hole           -> mkPart 1 3
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
toRole atI
    | atI ( 0, 0)                = [TileRole_Full]

    -- | atI (-1, 0) &&  atI ( 1, 0) = [TileRole_Cross Cross_TopLeftBottomRight]
    -- | atI ( 0,-1) &&  atI ( 0, 1) = [TileRole_Cross Cross_BottomLeftTopRight]

    | atI ( 1, 0) && atI (-1,-1) && atI ( 0, 1) = [TileRole_Hole]
    | atI (-1, 0) && atI ( 1, 1) && atI ( 0,-1) = [TileRole_Hole]
    | atI ( 0, 1) && atI ( 1,-1) && atI (-1, 0) = [TileRole_Hole]
    | atI ( 0,-1) && atI (-1, 1) && atI ( 1, 0) = [TileRole_Hole]

    | atI ( 1, 0) && atI ( 0, 1) = [TileRole_InnerCorner Corner_TopRight]
    | atI (-1, 0) && atI ( 0, 1) = [TileRole_InnerCorner Corner_TopLeft]
    | atI ( 1, 0) && atI ( 0,-1) = [TileRole_InnerCorner Corner_BottomRight]
    | atI (-1, 0) && atI ( 0,-1) = [TileRole_InnerCorner Corner_BottomLeft]

    | atI ( 1, 0) && atI (-1,-1) = [TileRole_InnerCorner Corner_BottomRight]
    | atI (-1, 0) && atI ( 1, 1) = [TileRole_InnerCorner Corner_TopLeft]
    | atI ( 0, 1) && atI ( 1,-1) = [TileRole_InnerCorner Corner_TopRight]
    | atI ( 0,-1) && atI (-1, 1) = [TileRole_InnerCorner Corner_BottomLeft]

    | atI ( 1, 0) && atI (-1, 1) = [TileRole_InnerCorner Corner_TopRight]
    | atI (-1, 0) && atI ( 1,-1) = [TileRole_InnerCorner Corner_BottomLeft]
    | atI ( 0, 1) && atI (-1,-1) = [TileRole_InnerCorner Corner_TopLeft]
    | atI ( 0,-1) && atI ( 1, 1) = [TileRole_InnerCorner Corner_BottomRight]

    | atI ( 1, 0)                = [TileRole_Edge Edge_Left]
    | atI (-1, 0)                = [TileRole_Edge Edge_Right]
    | atI ( 0, 1)                = [TileRole_Edge Edge_Bottom]
    | atI ( 0,-1)                = [TileRole_Edge Edge_Top]

    | otherwise
        = ifI ( 1, 1) (TileRole_OuterCorner Corner_BottomLeft)
        $ ifI (-1, 1) (TileRole_OuterCorner Corner_BottomRight)
        $ ifI ( 1,-1) (TileRole_OuterCorner Corner_TopLeft)
        $ ifI (-1,-1) (TileRole_OuterCorner Corner_TopRight)
        $ []
    where
    ifI i v = if atI i then (v:) else id

