module Entity.Wall
    ( Wall, wallToEntity
    ) where

import Delude

import Types.Entity.Wall
import Entity.Utils

import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color
import qualified Resource
import Resource (Resource)
import ResourceManager (lookupResource, ResourceMap)

--------------------------------------------------------------------------------

actOn :: Wall -> EntityAction -> Wall
actOn x _ = x

update :: Wall -> EntityContext -> (Maybe Wall, [DirectedEntityAction])
update x _ = (Just x, [])

render :: Wall -> RenderContext -> RenderAction
-- render x ctx = renderSprite ctx (Resource.mkEnvPart 40 11)
render x ctx = renderSprite ctx (Resource.mkEnvRect 36 10 2 2)
    & translate loc
    where
    Location loc = x^.location

oracle :: Wall -> EntityOracle
oracle x = def
   & location .~ Just (x^.location)
   & static   .~ True

renderSprite :: HasResources c ResourceMap => c -> Resource -> RenderAction
renderSprite ctx r = case lookupResource r $ ctx^.resources of
    Nothing  -> renderShape shape
    Just img -> scale (1/32) $ renderImg img
    where
    shape = def
        & shapeType   .~ SimpleSquare
        & color       .~ Color.opaque Color.gray

--------------------------------------------------------------------------------

wallToEntity :: Wall -> Entity
wallToEntity = makeEntity $ EntityParts
   { makeActOn  = actOn
   , makeUpdate = update
   , makeRender = render
   , makeOracle = oracle
   , makeSave   = EntitySum_Wall
   , makeKind   = EntityKind_Static
   }
