module EntityLike
    ( EntityLike (..)

    ) where

-- import Delude

import Types.Entity

import Entity.Agent
import Entity.Tile
import Entity.Effect
import Entity.Projectile
import Entity.Passive

--------------------------------------------------------------------------------

class EntityLike ent where toEntity :: ent -> Entity

instance EntityLike Tile         where toEntity =         tileToEntity
instance EntityLike Passive      where toEntity =      passiveToEntity
instance EntityLike Agent        where toEntity =        agentToEntity
instance EntityLike Effect       where toEntity =       effectToEntity
instance EntityLike Projectile   where toEntity =   projectileToEntity

instance EntityLike EntitySum where
    toEntity tag = case tag of
        EntitySum_Tile         x -> toEntity x
        EntitySum_Passive      x -> toEntity x
        EntitySum_Agent        x -> toEntity x
