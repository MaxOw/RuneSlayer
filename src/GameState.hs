module GameState
    ( module Types.GameState
    , zoomGameState
    , updateGameState
    , actOnEntity
    , actOnFocusedEntity
    , entityIdToWithId

    , toggleDebug, isDebugFlagOn
    , pickupItem, pickupAllItems
    , dropItem, dropAllItems
    ) where

import Delude
import qualified Data.Set as Set

import Engine (userState)
import Types (Game)
import Types.Entity (EntityWithId(..))
import Types.Entity.Common (EntityId)
import Types.St
import Types.GameState
import Types.Debug (DebugFlag(..))
import Types.EntityAction
import Focus

import qualified EntityIndex

--------------------------------------------------------------------------------

zoomGameState :: GameStateM a -> Game a
zoomGameState = zoom (userState.gameState)

updateGameState :: Game ()
updateGameState = zoomGameState $ do
    join $ EntityIndex.update
        <$> use actions
        <*> use frameCount
        <*> use entities

    actions .= []

addDirectedAction :: DirectedEntityAction -> Game ()
addDirectedAction a = userState.gameState.actions %= (a:)

actOnEntity :: EntityId -> EntityAction -> Game ()
actOnEntity eid = addDirectedAction . DirectedEntityAction eid

actOnFocusedEntity :: EntityAction -> Game ()
actOnFocusedEntity act = zoomGameState $ do
    use focusId >>= \case
        Just fi -> actions %= (DirectedEntityAction fi act:)
        Nothing -> return ()

entityIdToWithId :: EntityId -> Game (Maybe EntityWithId)
entityIdToWithId eid = zoomGameState $ do
    es <- use entities
    EntityIndex.lookupById eid es

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
    es <- fmap (view entityId) <$> focusItemsInRange
    mapM_ (flip actOnEntity $ EntityAction_SelfAddedBy fi) es

dropAllItems :: Game ()
dropAllItems = actOnFocusedEntity EntityAction_DropAllItems

