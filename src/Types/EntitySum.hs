module Types.EntitySum where

import Types.Entity.Player
import Types.Entity.Wall
import Types.Entity.Tile
import Types.Entity.Item
import Types.Entity.Container

data EntitySum
   = EntityPlayer    Player
   | EntityWall      Wall
   | EntityTile      Tile
   | EntityItem      Item
   | EntityContainer Container
