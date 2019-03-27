{-# Language TemplateHaskell #-}
module Types.Entity.Tile
    ( module Types.Entity.Tile
    , module Types.Entity.TileSet
    ) where

import Delude

import Types.Entity.Common (Location)
import Types.Entity.TileSet

--------------------------------------------------------------------------------

data Tile = Tile
   { tile_location :: Location
   , tile_tileSet  :: TileSet
   , tile_role     :: TileRole
   } deriving (Generic)
instance Default Tile
makeFieldsCustom ''Tile

instance GetZIndex Tile Word32 where
    get_zindex = view (tileSet.zindex)
