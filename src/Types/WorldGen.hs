module Types.WorldGen where

import Delude
import Engine.Common.Types (Size)
import Types.Entity (Entity)
import Data.Vector (Vector)
-- import Engine (TextureBuffer)
import Codec.Picture (DynamicImage)

data WorldGenConfig = WorldGenConfig
   { field_size          :: Size Float
   , field_seed          :: Int
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
        { field_size = pure 100
        , field_seed = 29
        }

instance Default WorldGenOutput
