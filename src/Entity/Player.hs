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
actOn x a = case a of
    EntityAction_SetMoveVector   v -> setMoveVector   v x
    EntityAction_ToggleDebug     f -> toggleDebugFlag f x
    EntityAction_DropAllItems      -> handleOnUpdate  a x
    EntityAction_AddItem         _ -> handleOnUpdate  a x
    EntityAction_DropItem        _ -> handleOnUpdate  a x
    EntityAction_OwnerDropItem   _ -> handleOnUpdate  a x
    _ -> x

update :: Player -> EntityContext -> (Maybe Player, [DirectedEntityAction])
update x ctx = runUpdate x ctx $ do
    integrateLocation
    anyMatch _EntityAction_AddItem  addItems
    anyMatch _EntityAction_DropAllItems dropAllItems
    mapM_ processAction =<< use (self.processOnUpdate)
    self.processOnUpdate .= mempty

processAction :: EntityAction -> Update Player ()
processAction = \case
    EntityAction_DropItem i -> dropItem i
    EntityAction_OwnerDropItem i -> dropItemAction i
    _ -> return ()

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
