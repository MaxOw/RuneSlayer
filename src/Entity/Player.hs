module Entity.Player
    ( Player, playerToEntity
    ) where

import Delude

import Types.Entity
import Types.Entity.Player
import Entity.Utils
import Entity.Actions

import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color

--------------------------------------------------------------------------------

actOn :: Player -> EntityAction -> Player
actOn p a = case a of
    EntityAction_SetMoveVector   v -> setMoveVector v p
    EntityAction_ToggleDebug     f -> toggleDebugFlag f p
    EntityAction_AddToInventory  _ -> handleOnUpdate a p
    EntityAction_DropAllItems      -> handleOnUpdate a p
    _ -> p

update :: Player -> EntityContext -> (Maybe Player, [DirectedEntityAction])
update p ctx = runUpdate p ctx $ do
    integrateLocation
    anyMatch _EntityAction_AddToInventory addItems
    anyMatch _EntityAction_DropAllItems   dropAllItems
    self.processOnUpdate .= mempty

--------------------------------------------------------------------------------

render :: Player -> RenderAction
render x = renderComposition
    [ renderDebug
    , renderShape shape & scale 0.3
    ] & translate loc
    where
    loc = x^.location._Wrapped
    shape = def
        & shapeType   .~ SimpleCircle
        & color       .~ Color.opaque Color.blue

    renderDebug
        = renderComposition $ map snd
        $ filter (\(f, _) -> x^.debugFlags.f)
        [ (drawPickupRange, renderPickupRange)
        ]

    rangeScale = defaultPickupRange^._Wrapped * 2
    renderPickupRange = scale rangeScale $ renderShape $ def
        & shapeType   .~ SimpleCircle
        & color       .~ Color.withOpacity Color.red 0.3

thisOracle :: Player -> EntityOracle
thisOracle x = def
   & location  .~ Just (x^.location)
   & equipment .~ Just (x^.equipment)

--------------------------------------------------------------------------------

playerToEntity :: Player -> Entity
playerToEntity = makeEntity $ EntityParts
   { makeActOn  = actOn
   , makeUpdate = update
   , makeRender = render
   , makeOracle = thisOracle
   , makeSave   = EntityPlayer
   }
