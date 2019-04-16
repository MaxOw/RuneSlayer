module Types.WorldGen where

import Delude
import Engine.Common.Types (Size)
import Types.Entity (Entity)
import Types.Entity.TileSet (TileSetName)
import Data.Vector (Vector)
-- import Engine (TextureBuffer)
import Codec.Picture (DynamicImage)

data WorldGenConfig = WorldGenConfig
   { field_size            :: Size Float
   , field_seed            :: Int
   , field_baseTileSet     :: Maybe TileSetName
   , field_baseLandTileSet :: Maybe TileSetName
   } deriving (Generic)

data WorldGenOutput = WorldGenOutput
   { field_entities      :: Vector Entity
   , field_overviewImage :: Maybe DynamicImage
   } deriving (Generic)

overviewImage :: Lens' WorldGenOutput (Maybe DynamicImage)
overviewImage = ff#overviewImage

--------------------------------------------------------------------------------

instance Default WorldGenConfig where
    def = WorldGenConfig
        { field_size            = pure 100
        , field_seed            = 29
        , field_baseTileSet     = def
        , field_baseLandTileSet = def
        }
instance FromJSON WorldGenConfig where
    parseJSON = genericParseJSON  customOptionsJSON

instance Default WorldGenOutput
