{-# Language TemplateHaskell #-}
module Types.ResourceManager (module Types.ResourceManager) where

import Delude
import Engine (Img)
import Engine.Common.Types (Size, Rect)
import Types.Sprite as Types.ResourceManager
import Types.Entity.StaticEntity
import Types.Entity.TileSet
import Types.Entity.Animation
import Types.Entity.ItemType
import Types.Entity.Unit

data Resources = Resources
   { resources_resourceMap   :: HashMap Text Img
   , resources_staticMap     :: HashMap StaticEntityTypeName StaticEntityType
   , resources_tileSetMap    :: HashMap TileSetName TileSet
   , resources_spriteMap     :: HashMap SpriteName SpriteDesc
   , resources_itemsMap      :: HashMap ItemTypeName ItemType
   , resources_unitsMap      :: HashMap UnitTypeName UnitType
   , resources_animationsMap :: HashMap Text AnimationDesc
   } deriving (Generic)
instance Default Resources
instance HasResources Resources Resources where resources = id

makeFieldsCustom ''Resources
