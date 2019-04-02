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

data TileRole
   = TileRole_Full
   -- | TileRole_Path
   -- | TileRole_Hole
   | TileRole_Edge        Edge
   | TileRole_OuterCorner Corner
   | TileRole_InnerCorner Corner
   | TileRole_Cross Cross
   deriving (Eq)

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
   { field_name     :: TileSetName
   , field_desc     :: TileSetDesc
   , field_zindex   :: Word32
   } deriving (Generic, Show)

namedTileSet :: Text -> TileSet
namedTileSet n = TileSet
   { field_name     = TileSetName n
   , field_desc     = TileSetDesc_Standard def
   , field_zindex   = 1
   }

--------------------------------------------------------------------------------

instance Default TileSet where
    def = TileSet
        { field_name   = TileSetName ""
        , field_desc   = TileSetDesc_Custom ()
        , field_zindex = 0
        }

instance ToJSON TileSetDesc where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON TileSetDesc where
    parseJSON = genericParseJSON customOptionsJSON

instance ToJSON TileSet where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON TileSet where
    parseJSON = genericParseJSON customOptionsJSON


