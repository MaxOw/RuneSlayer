module EntityLike
    ( EntityLike (..)

    ) where

-- import Delude

import Types.Entity

import Entity.Player
import Entity.Wall
import Entity.Container
import Entity.Item

--------------------------------------------------------------------------------

class EntityLike ent where toEntity :: ent -> Entity

instance EntityLike Player    where toEntity =    playerToEntity
instance EntityLike Wall      where toEntity =      wallToEntity
instance EntityLike Container where toEntity = containerToEntity
instance EntityLike Item      where toEntity =      itemToEntity

instance EntityLike EntitySum where
    toEntity tag = case tag of
        EntityPlayer    x -> toEntity x
        EntityWall      x -> toEntity x
        EntityContainer x -> toEntity x
        EntityItem      x -> toEntity x

