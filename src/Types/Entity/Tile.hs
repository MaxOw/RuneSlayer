{-# Language TemplateHaskell #-}
module Types.Entity.Tile where

import Delude

import Types.ResourceManager (Resource)
import Types.Entity.Common (Location)

--------------------------------------------------------------------------------

data TileType = TileType
   { tileType_resource :: Maybe Resource
   } deriving (Generic)
instance Default TileType
makeFieldsCustom ''TileType

data Tile = Tile
   { tile_location :: Location
   , tile_tileType :: TileType
   } deriving (Generic)
instance Default Tile
makeFieldsCustom ''Tile
