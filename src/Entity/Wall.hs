module Entity.Wall
    ( Wall, wallToEntity
    ) where

import Delude

import Types.Entity.Wall
import Entity.Utils

import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color

--------------------------------------------------------------------------------

actOn :: Wall -> EntityAction -> Wall
actOn x _ = x

update :: Wall -> EntityContext -> (Maybe Wall, [DirectedEntityAction])
update x _ = (Just x, [])

render :: Wall -> RenderContext -> RenderAction
render x _ctx = renderShape shape
    & translate loc
    where
    Location loc = x^.location
    shape = def
        & shapeType   .~ SimpleSquare
        & color       .~ Color.opaque Color.gray

oracle :: Wall -> EntityOracle
oracle x = def
   & location .~ Just (x^.location)

--------------------------------------------------------------------------------

wallToEntity :: Wall -> Entity
wallToEntity = makeEntity $ EntityParts
   { makeActOn  = actOn
   , makeUpdate = update
   , makeRender = render
   , makeOracle = oracle
   , makeSave   = EntityWall
   }
