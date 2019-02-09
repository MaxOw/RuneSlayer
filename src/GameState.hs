module GameState
    ( module Types.GameState
    , zoomGameState
    , updateGameState
    , actOnEntity
    , actOnFocusedEntity
    , addEntityAndFocus
    , addEntity
    , entityIdToWithId

    , toggleDebug, isDebugFlagOn
    , pickupItem, pickupAllItems
    , dropItem, dropAllItems
    ) where

import Delude
import qualified Data.Set as Set

import Engine (userState)
import Types (Game)
import Types.Entity (Entity, EntityWithId(..))
import Types.Entity.Common (EntityId)
import Types.St
import Types.GameState
import Types.Debug (DebugFlag(..))
import Types.EntityAction
import Focus

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

--------------------------------------------------------------------------------

toggleDebug :: DebugFlag -> Game ()
toggleDebug x = do
    userState.debugFlags %= toggleSet x
    case x of
      DebugFlag_DrawPickupRange -> debugFocus EntityDebugFlag_DrawPickupRange
      _ -> return ()
    where
    debugFocus = actOnFocusedEntity . EntityAction_ToggleDebug
    -- toggleShowScroller =

isDebugFlagOn :: DebugFlag -> Game Bool
isDebugFlagOn x = uses (userState.debugFlags) (Set.member x)

pickupItem :: EntityId -> Game ()
pickupItem eid = withFocusId $ actOnEntity eid . EntityAction_SelfAddedBy

dropItem :: EntityId -> Game ()
dropItem = actOnFocusedEntity . EntityAction_DropItem

pickupAllItems :: Game ()
pickupAllItems = withFocusId $ \fi -> do
    es <- fmap (view entityId) <$> liftGame focusItemsInRange
    mapM_ (flip actOnEntity $ EntityAction_SelfAddedBy fi) es

dropAllItems :: Game ()
dropAllItems = actOnFocusedEntity EntityAction_DropAllItems

