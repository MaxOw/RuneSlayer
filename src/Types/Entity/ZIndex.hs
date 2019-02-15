module Types.Entity.ZIndex
    ( TileZIndex
    , EntityZIndex (..)
    , toZIndex
    ) where

import Delude

newtype TileZIndex = TileZIndex { unTileZIndex :: Word32 }
    deriving (Num, Eq, Ord)
instance Bounded TileZIndex where
    minBound = 0
    maxBound = 1000

data EntityZIndex
   = EntityZIndex_Tile TileZIndex
   | EntityZIndex_Item
   | EntityZIndex_Vertical

class ToZIndex a where
    toZIndex :: a -> Word32

instance ToZIndex TileZIndex where
    toZIndex = unTileZIndex . min maxBound

instance ToZIndex EntityZIndex where
    toZIndex = \case
        EntityZIndex_Tile z   -> toZIndex z
        EntityZIndex_Item     -> 1 + maxTileZIndex
        EntityZIndex_Vertical -> 2 + maxTileZIndex
        where
        maxTileZIndex = toZIndex (EntityZIndex_Tile maxBound)
