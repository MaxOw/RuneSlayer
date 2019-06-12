module Types.EntitySum where

import Types.Entity.Player
import Types.Entity.Tile
import Types.Entity.Item
import Types.Entity.StaticEntity
import Types.Entity.Unit
-- import Types.Entity.Projectile

data EntitySum
   = EntitySum_Player       Player
   | EntitySum_Tile         Tile
   | EntitySum_Item         Item
   | EntitySum_StaticEntity StaticEntity
   | EntitySum_Unit         Unit
   -- | EntitySum_Projectile   Projectile
