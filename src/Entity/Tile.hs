module Entity.Tile
    ( Tile, tileToEntity

    , makeTile
    ) where

import Delude
import Data.Hashable (hash)

import Types.Entity.Tile
import Entity.Utils
import Entity.Actions

import Entity.TileSet (selectTile)
import ResourceManager (renderSprite)

--------------------------------------------------------------------------------

actOn :: Tile -> EntityAction -> Tile
actOn x _ = x

update :: Tile -> EntityContext -> Q (Maybe Tile, [DirectedAction])
update x _ = return (Just x, [])

render :: Tile -> RenderContext -> RenderAction
render x ctx
    = withZIndex x $ locate x
    $ renderSprite rs $ selectTile rndSeed (x^.role) (x^.tileSet)
    where
    rs = ctx^.resources
    rndSeed = hash $ x^.location._Wrapped

oracle :: Tile -> EntityQuery a -> Maybe a
oracle x = \case
    EntityQuery_Location -> Just $ x^.location
    _                    -> Nothing

--------------------------------------------------------------------------------

tileToEntity :: Tile -> Entity
tileToEntity = makeEntity $ EntityParts
   { makeActOn  = actOn
   , makeUpdate = update
   , makeRender = render
   , makeOracle = oracle
   , makeSave   = Just . EntitySum_Tile
   , makeKind   = const EntityKind_Tile
   }

makeTile :: TileRole -> TileSet -> Tile
makeTile tr ts = def { field_tileSet = ts, field_role = tr }

