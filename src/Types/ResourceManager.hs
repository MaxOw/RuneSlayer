module Types.ResourceManager (module Types.ResourceManager) where

import Delude
import Engine (Img)
import Types.Sprite as Types.ResourceManager
import Types.Entity.StaticEntity
import Types.Entity.TileSet
import Types.Entity.Animation
import Types.Entity.ItemType
import Types.Entity.Unit

data Resources = Resources
   { field_resourceMap   :: HashMap Text Img
   , field_spriteMap     :: HashMap Text SpriteDesc
   , field_staticMap     :: HashMap StaticEntityTypeName StaticEntityType
   , field_tileSetMap    :: HashMap TileSetName TileSet
   , field_itemsMap      :: HashMap ItemTypeName ItemType
   , field_unitsMap      :: HashMap UnitTypeName UnitType
   , field_animationsMap :: HashMap Text AnimationDesc
   } deriving (Generic)
instance Default Resources
instance HasResources Resources Resources where resources = id


