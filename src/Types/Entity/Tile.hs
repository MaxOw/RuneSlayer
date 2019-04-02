module Types.Entity.Tile
    ( module Types.Entity.Tile
    , module Types.Entity.TileSet
    ) where

import Delude

import Types.Entity.Common (Location)
import Types.Entity.TileSet

--------------------------------------------------------------------------------

data Tile = Tile
   { field_location :: Location
   , field_tileSet  :: TileSet
   , field_role     :: TileRole
   } deriving (Generic)
instance Default Tile


instance GetZIndex Tile Word32 where
    get_zindex = view (tileSet.zindex)
