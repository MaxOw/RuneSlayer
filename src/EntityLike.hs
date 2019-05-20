module EntityLike
    ( EntityLike (..)

    ) where

-- import Delude

import Types.Entity

import Entity.Player
import Entity.Tile
import Entity.Item
import Entity.StaticEntity
import Entity.Unit
import Entity.Effect

--------------------------------------------------------------------------------

class EntityLike ent where toEntity :: ent -> Entity

instance EntityLike Player       where toEntity =       playerToEntity
instance EntityLike Tile         where toEntity =         tileToEntity
instance EntityLike Item         where toEntity =         itemToEntity
instance EntityLike StaticEntity where toEntity = staticEntityToEntity
instance EntityLike Unit         where toEntity =         unitToEntity
instance EntityLike Effect       where toEntity =       effectToEntity

instance EntityLike EntitySum where
    toEntity tag = case tag of
        EntitySum_Player       x -> toEntity x
        EntitySum_Tile         x -> toEntity x
        EntitySum_Item         x -> toEntity x
        EntitySum_StaticEntity x -> toEntity x
        EntitySum_Unit         x -> toEntity x
