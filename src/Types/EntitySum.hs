module Types.EntitySum where

import Types.Entity.Player
import Types.Entity.Tile
import Types.Entity.Passive
import Types.Entity.Unit
-- import Types.Entity.Projectile

data EntitySum
   = EntitySum_Player       Player
   | EntitySum_Tile         Tile
   | EntitySum_Passive      Passive
   | EntitySum_Unit         Unit
   -- | EntitySum_Projectile   Projectile
