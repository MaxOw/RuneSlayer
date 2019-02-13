module Entity.Tile
    ( Tile, tileToEntity

    , makeTile, makeSimpleTile
    ) where

import Delude

import Types.Entity.Tile
import Entity.Utils

import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color
import Resource (Resource)
import ResourceManager (lookupResource, ResourceMap)

--------------------------------------------------------------------------------

actOn :: Tile -> EntityAction -> Tile
actOn x _ = x

update :: Tile -> EntityContext -> (Maybe Tile, [DirectedEntityAction])
update x _ = (Just x, [])

render :: Tile -> RenderContext -> RenderAction
-- render x ctx = renderSprite ctx (Resource.mkEnvPart 40 11)
render x ctx = loc $ case x^.tileType.resource of
    Nothing -> mempty
    Just rs -> renderSprite ctx rs
    where
    loc = translate (x^.location._Wrapped)

oracle :: Tile -> EntityOracle
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

tileToEntity :: Tile -> Entity
tileToEntity = makeEntity $ EntityParts
   { makeActOn  = actOn
   , makeUpdate = update
   , makeRender = render
   , makeOracle = oracle
   , makeSave   = EntityTile
   }

makeTile :: TileType -> Tile
makeTile tt = set tileType tt def

makeSimpleTile :: Resource -> Tile
makeSimpleTile r = makeTile $ def
    & resource .~ Just r

