module GameState
    ( module Types.GameState
    , zoomGameState
    , updateGameState
    , actOnEntity
    , actOnFocusedEntity
    , addEntityAndFocus
    , addEntity
    , entityIdToWithId
    ) where

import Delude

import Engine (userState)
import Types (Game)
import Types.Entity (Entity, EntityWithId(..))
import Types.Entity.Common (EntityId)
import Types.St
import Types.GameState
import Types.EntityAction

import EntityIndex

--------------------------------------------------------------------------------

zoomGameState :: GameStateM a -> Game a
zoomGameState = zoom (userState.gameState)

updateGameState :: Game ()
updateGameState = zoomGameState $ do
    as <- use actions
    es <- use entities
    fct <- use frameCount
    assign entities =<< updateIndex as fct es
    -- entities %= updateIndex as
    actions  .= []

addDirectedAction :: DirectedEntityAction -> Game ()
addDirectedAction a = userState.gameState.actions %= (a:)

actOnEntity :: EntityId -> EntityAction -> Game ()
actOnEntity eid = addDirectedAction . DirectedEntityAction eid

actOnFocusedEntity :: EntityAction -> Game ()
actOnFocusedEntity act = zoomGameState $ do
    use focusId >>= \case
        Just fi -> actions %= (DirectedEntityAction fi act:)
        Nothing -> return ()

addEntityAndFocus :: Entity -> GameState -> GameState
addEntityAndFocus ent g = set focusId (getLastId $ ng^.entities) ng
    where
    ng = addEntity ent g

addEntity :: Entity -> GameState -> GameState
addEntity e g = over entities (addToIndex e) g

entityIdToWithId :: EntityId -> Game (Maybe EntityWithId)
entityIdToWithId eid = zoomGameState $ do
    es <- use entities
    return $ EntityWithId eid <$> lookupEntityById eid es

