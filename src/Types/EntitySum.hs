module Types.EntitySum where

import Types.Entity.Agent
import Types.Entity.Tile
import Types.Entity.Passive
-- import Types.Entity.Projectile

data EntitySum
   = EntitySum_Agent        Agent
   | EntitySum_Tile         Tile
   | EntitySum_Passive      Passive
   -- | EntitySum_Projectile   Projectile
