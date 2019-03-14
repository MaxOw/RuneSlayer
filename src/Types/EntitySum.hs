module Types.EntitySum where

import Types.Entity.Player
import Types.Entity.Tile
import Types.Entity.Item
import Types.Entity.Container
import Types.Entity.StaticEntity
import Types.Entity.Unit

data EntitySum
   = EntitySum_Player       Player
   | EntitySum_Tile         Tile
   | EntitySum_Item         Item
   | EntitySum_Container    Container
   | EntitySum_StaticEntity StaticEntity
   | EntitySum_Unit         Unit
