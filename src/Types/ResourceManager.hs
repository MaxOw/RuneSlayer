module Types.ResourceManager (module Types.ResourceManager) where

import Delude
import Engine (Img)
import Types.Sprite as Types.ResourceManager
import Types.Entity.StaticEntity
import Types.Entity.TileSet
import Types.Entity.Animation
import Types.Entity.Item
import Types.Entity.Unit
import Types.Skills.Runes

data Resources = Resources
   { field_imgMap        :: HashMap Text Img
   , field_spriteMap     :: HashMap Text SpriteDesc
   , field_staticMap     :: HashMap StaticEntityTypeName StaticEntityType
   , field_tileSetMap    :: HashMap TileSetName TileSet
   , field_itemsMap      :: HashMap ItemTypeName ItemType
   , field_unitsMap      :: HashMap UnitTypeName UnitType
   , field_animationsMap :: HashMap AnimationName Animation
   , field_runeSet       :: RuneSet
   } deriving (Generic)
instance Default Resources
instance HasResources Resources Resources where resources = id

imgMap :: Lens' Resources (HashMap Text Img)
imgMap = ff#imgMap

