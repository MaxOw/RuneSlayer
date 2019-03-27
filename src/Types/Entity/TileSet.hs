{-# Language TemplateHaskell #-}
module Types.Entity.TileSet where

import Delude
import Types.Sprite (SpriteDesc)

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

data Cross
   = Cross_TopLeftBottomRight
   | Cross_BottomLeftTopRight
   deriving (Eq, Enum, Bounded)

data Level
   = Level_Vertical
   | Level_Horizontal
   deriving (Eq, Enum, Bounded)

data TileRole
   = TileRole_Full
   | TileRole_Path
   | TileRole_Hole
   | TileRole_Edge        Edge
   | TileRole_OuterCorner Corner
   | TileRole_InnerCorner Corner
   | TileRole_Cross Cross
   -- | TileRole_SemiCross Level Corner
   deriving (Eq)
   -- deriving (Enum, Bounded)

instance Default TileRole where def = TileRole_Full

--------------------------------------------------------------------------------

-- newtype CustomTileSet = CustomTileSet { unCustomTileSet :: [TileDesc] }

data TileSetDesc
   = TileSetDesc_Standard SpriteDesc
   | TileSetDesc_Custom () -- CustomTileSet
    deriving (Show, Generic)

newtype TileSetName = TileSetName { unTileSetName :: Text }
    deriving (Show, Generic, Eq, Hashable, ToJSON, FromJSON)
data TileSet = TileSet
   { tileSet_name     :: TileSetName
   , tileSet_desc     :: TileSetDesc
   , tileSet_zindex   :: Word32
   } deriving (Generic, Show)

namedTileSet :: Text -> TileSet
namedTileSet n = TileSet
   { tileSet_name     = TileSetName n
   , tileSet_desc     = TileSetDesc_Standard def
   , tileSet_zindex   = 1
   }

--------------------------------------------------------------------------------

instance Default TileSet where
    def = TileSet
        { tileSet_name   = TileSetName ""
        , tileSet_desc   = TileSetDesc_Custom ()
        , tileSet_zindex = 0
        }

instance ToJSON TileSetDesc where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON TileSetDesc where
    parseJSON = genericParseJSON customOptionsJSON

instance ToJSON TileSet where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON TileSet where
    parseJSON = genericParseJSON customOptionsJSON

makeFieldsCustom ''TileSet
