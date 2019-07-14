module EntityLike
    ( EntityLike (..)

    ) where

-- import Delude

import Types.Entity

import Entity.Player
import Entity.Tile
import Entity.Unit
import Entity.Effect
import Entity.Projectile
import Entity.Passive

--------------------------------------------------------------------------------

class EntityLike ent where toEntity :: ent -> Entity

instance EntityLike Player       where toEntity =       playerToEntity
instance EntityLike Tile         where toEntity =         tileToEntity
instance EntityLike Passive      where toEntity =      passiveToEntity
instance EntityLike Unit         where toEntity =         unitToEntity
instance EntityLike Effect       where toEntity =       effectToEntity
instance EntityLike Projectile   where toEntity =    projectileToEntity

instance EntityLike EntitySum where
    toEntity tag = case tag of
        EntitySum_Player       x -> toEntity x
        EntitySum_Tile         x -> toEntity x
        EntitySum_Passive      x -> toEntity x
        EntitySum_Unit         x -> toEntity x
