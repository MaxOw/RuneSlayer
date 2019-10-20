module Types.Entity.TileSet where

import Delude
import qualified Data.Map as Map
import Types.Sprite (SpriteDesc)
import Types.Collider (Shape, CollideWith)
import Color (ColorDesc)

data Edge
   = Edge_Bottom
   | Edge_Left
   | Edge_Top
   | Edge_Right
   deriving (Eq, Ord, Enum, Bounded, Generic)

data Corner
   = Corner_BottomRight
   | Corner_BottomLeft
   | Corner_TopLeft
   | Corner_TopRight
   deriving (Eq, Ord, Enum, Bounded, Generic)

data Cross
   = Cross_TopLeftBottomRight
   | Cross_BottomLeftTopRight
   deriving (Eq, Ord, Enum, Bounded, Generic)

data TileRole
   = TileRole_Full
   -- | TileRole_Path
   -- | TileRole_Hole
   | TileRole_Edge        Edge
   | TileRole_OuterCorner Corner
   | TileRole_InnerCorner Corner
   | TileRole_Cross Cross
   deriving (Eq, Ord, Generic)

instance Default TileRole where def = TileRole_Full

--------------------------------------------------------------------------------

-- newtype CustomTileSet = CustomTileSet { unCustomTileSet :: [TileDesc] }

data TileSetDesc
   = TileSetDesc_Standard SpriteDesc
   | TileSetDesc_Custom () -- CustomTileSet
    deriving (Show, Generic)

newtype TileSetShapes = TileSetShapes (Map TileRole Shape)

newtype TileSetName = TileSetName { unTileSetName :: Text }
    deriving (Show, Generic, Eq, Hashable, FromJSON)
data TileSet = TileSet
   { field_name            :: TileSetName
   , field_color           :: Maybe ColorDesc
   -- ^ Pixel color on map overview miniature.
   , field_desc            :: TileSetDesc
   , field_collisionShapes :: TileSetShapes
   , field_collisionBits   :: BitSet32 CollideWith
   , field_zindex          :: Word32
   } deriving (Generic)

--------------------------------------------------------------------------------

instance Default TileSet where
    def = TileSet
        { field_name            = TileSetName ""
        , field_color           = def
        , field_desc            = TileSetDesc_Custom ()
        , field_collisionShapes = TileSetShapes def
        , field_collisionBits   = def
        , field_zindex          = 0
        }

instance FromJSON TileSetDesc where parseJSON = genericParseJSON customOptionsJSON
instance FromJSON TileSet     where parseJSON = genericParseJSON customOptionsJSON
instance FromJSON Edge        where parseJSON = genericParseJSON customOptionsJSON
instance FromJSON Corner      where parseJSON = genericParseJSON customOptionsJSON
instance FromJSON Cross       where parseJSON = genericParseJSON customOptionsJSON
instance FromJSON TileRole    where parseJSON = genericParseJSON customOptionsJSON

data Entry k v = Entry { key :: k, value :: v } deriving (Generic)
instance (FromJSON k, FromJSON v) => FromJSON (Entry k v) where
    parseJSON = genericParseJSON customOptionsJSON

instance FromJSON TileSetShapes where
    parseJSON x = TileSetShapes . Map.fromList . map fromEntry <$> parseJSON x
        where fromEntry (Entry k v) = (k, v)


