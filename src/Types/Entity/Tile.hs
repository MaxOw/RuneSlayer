{-# Language TemplateHaskell #-}
module Types.Entity.Tile
    ( module Types.Entity.Tile
    , module Types.Entity.TileType
    ) where

import Delude

import Types.Entity.Common (Location)
import Types.Entity.TileType
import Types.Entity.ZIndex

--------------------------------------------------------------------------------

data TileN t = Tile
   { tile_location :: Location
   , tile_tileType :: t
   } deriving (Generic)
instance Default t => Default (TileN t)
makeFieldsCustom ''TileN

type Tile     = TileN TileType
type TileNorm = TileN TileName

instance GetZIndex Tile Word32 where
    get_zindex = toZIndex . view (tileType.zindex)
