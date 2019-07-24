module Types.WorldGen where

import Delude
import Engine.Common.Types (Size)
import Types.Entity (Entity)
import Types.Entity.TileSet (TileSetName)
import Types.Entity.Passive (PassiveTypeName)
import Types.Entity.Agent   (AgentTypeName)
import Types.EntityAction   (EntityAction)
import Data.Vector (Vector)
import Codec.Picture (DynamicImage)

data CoveringLayer = CoveringLayer
   { field_tileset :: TileSetName
   , field_statics :: [ PassiveTypeName ]
   } deriving (Generic)

data Spawn n = Spawn
   { field_name    :: n
   , field_actions :: Maybe [EntityAction]
   } deriving (Generic)

data WorldGenConfig = WorldGenConfig
   { field_size            :: Size Float
   , field_seed            :: Int
   , field_baseTileSet     :: Maybe TileSetName
   , field_baseLandTileSet :: Maybe TileSetName
   , field_coveringLayers  :: [CoveringLayer]
   , field_items           :: Maybe [Spawn PassiveTypeName]
   , field_units           :: Maybe [Spawn AgentTypeName]
   } deriving (Generic)
instance HasSize WorldGenConfig (Size Float)

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
        , field_coveringLayers  = def
        , field_items           = def
        , field_units           = def
        }

instance FromJSON CoveringLayer where
    parseJSON = genericParseJSON  customOptionsJSON
instance FromJSON WorldGenConfig where
    parseJSON = genericParseJSON  customOptionsJSON
instance FromJSON n => FromJSON (Spawn n) where
    parseJSON = genericParseJSON  customOptionsJSON

instance Default WorldGenOutput
