module Types.ResourceManager (module Types.ResourceManager) where

import Delude
import Engine (Img)
import Types.Sprite as Types.ResourceManager
import Types.Entity.TileSet
import Types.Entity.Animation
import Types.Entity.Passive
import Types.Entity.Agent
import Types.Skills.Runes

data Resources = Resources
   { field_imgMap        :: HashMap Text Img
   , field_spriteMap     :: HashMap Text SpriteDesc
   , field_passiveMap    :: HashMap PassiveTypeName PassiveType
   , field_tileSetMap    :: HashMap TileSetName TileSet
   , field_agentsMap     :: HashMap AgentTypeName AgentType
   , field_animationsMap :: HashMap AnimationName Animation
   , field_runeSet       :: RuneSet
   } deriving (Generic)
instance Default Resources
instance HasResources Resources Resources where resources = id

imgMap :: Lens' Resources (HashMap Text Img)
imgMap = ff#imgMap

agentsMap :: Lens' Resources (HashMap AgentTypeName AgentType)
agentsMap = ff#agentsMap

passiveMap :: Lens' Resources (HashMap PassiveTypeName PassiveType)
passiveMap = ff#passiveMap
