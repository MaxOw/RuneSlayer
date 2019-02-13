module Types.Entity.TileType where

import Delude

data Edge
   = Edge_Bottom
   | Edge_Left
   | Edge_Top
   | Edge_Right
   deriving (Eq, Enum, Bounded)

data Corner
   = Corner_BottomRight
   | Corner_BottomLeft
   | Corner_TopLeft
   | Corner_TopRight
   deriving (Eq, Enum, Bounded)

data TileRole
   = TileRole_Full
   | TileRole_Path
   | TileRole_Hole
   | TileRole_Edge        Edge
   | TileRole_OuterCorner Corner
   | TileRole_InnerCorner Corner
   -- deriving (Enum, Bounded)

