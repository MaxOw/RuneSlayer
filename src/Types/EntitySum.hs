module Types.EntitySum where

import Types.Entity.Player
import Types.Entity.Wall
import Types.Entity.Tile
import Types.Entity.Item
import Types.Entity.Container
import Types.Entity.StaticEntity

data EntitySum
   = EntitySum_Player       Player
   | EntitySum_Wall         Wall
   | EntitySum_Tile         Tile
   | EntitySum_Item         Item
   | EntitySum_Container    Container
   | EntitySum_StaticEntity StaticEntity
