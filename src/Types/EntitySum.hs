module Types.EntitySum where

import Types.Entity.Player
import Types.Entity.Wall
import Types.Entity.Item
import Types.Entity.Container

data EntitySum
   = EntityPlayer    Player
   | EntityWall      Wall
   | EntityItem      Item
   | EntityContainer Container
