{-# Language TemplateHaskell #-}
module Types.Entity.TileType where

import Delude
import Types.ResourceManager (Resource)
import Types.Entity.ZIndex

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
   deriving (Eq)
   -- deriving (Enum, Bounded)

--------------------------------------------------------------------------------

newtype TileName = TileName { unTileName :: Text }
data TileType = TileType
   { tileType_name     :: TileName
   , tileType_sprite   :: Maybe Resource
   , tileType_zindex   :: TileZIndex
   } deriving (Generic)
makeFieldsCustom ''TileType

namedTileType :: Text -> TileType
namedTileType n = TileType
   { tileType_name     = TileName n
   , tileType_sprite   = Nothing
   , tileType_zindex   = 1
   }

