module Entity.Tile
    ( Tile, tileToEntity

    , makeTile, makeSimpleTile, makeSimpleFullTile
    ) where

import Delude

import Types.Entity.Tile
import Entity.Utils
import Entity.Actions

import Resource (Resource)

--------------------------------------------------------------------------------

actOn :: Tile -> EntityAction -> Tile
actOn x _ = x

update :: Tile -> EntityContext -> Q (Maybe Tile, [DirectedAction])
update x _ = return (Just x, [])

render :: Tile -> RenderContext -> RenderAction
-- render x ctx = renderSprite ctx (Resource.mkEnvPart 40 11)
render x ctx = withZIndex x $ locate x $ case x^.tileType.sprite of
    Nothing -> mempty
    Just rs -> renderResource ctx rs

oracle :: Tile -> EntityOracle
oracle x = def
   & location .~ Just (x^.location)

--------------------------------------------------------------------------------

tileToEntity :: Tile -> Entity
tileToEntity = makeEntity $ EntityParts
   { makeActOn  = actOn
   , makeUpdate = update
   , makeRender = render
   , makeOracle = oracle
   , makeSave   = EntitySum_Tile
   , makeKind   = EntityKind_Tile
   }

makeTile :: TileType -> Tile
makeTile tt = (def :: TileN ()) { tile_tileType = tt }

makeSimpleTile :: Resource -> Tile
makeSimpleTile r = makeTile $ namedTileType "Simple"
    & sprite .~ Just r

makeSimpleFullTile :: Resource -> Tile
makeSimpleFullTile r = makeTile $ namedTileType "Simple"
    & sprite .~ Just r
    & zindex   .~ 0

