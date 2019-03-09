module GameState
    ( module Types.GameState
    , zoomGameState
    , updateGameState
    , actOnEntity
    , actOnFocusedEntity

    , toggleDebug, isDebugFlagOn
    , debugRunAnimation
    , pickupItem, pickupAllItems
    , dropItem, dropAllItems
    , executeAttack
    ) where

import Delude
import qualified Data.Set as Set

import Engine (userState)
import Types (Game)
import Types.Entity (Entity)
import Types.Entity.Common (EntityId)
import Types.Entity.Animation (AnimationKind)
import Types.St
import Types.GameState
import Types.Debug (DebugFlag(..))
import Types.EntityAction
import Types.DirectedAction
import Focus

import EntityLike (toEntity)
import Entity.Item (makeItem)

import qualified EntityIndex

--------------------------------------------------------------------------------

zoomGameState :: GameStateM a -> Game a
zoomGameState = zoom (userState.gameState)

updateGameState :: Game ()
updateGameState = zoomGameState $ do
    join $ EntityIndex.update
        <$> pure handleWorldAction
        <*> use actions
        <*> use frameCount
        <*> use entities

    actions .= []

handleWorldAction :: WorldAction -> GameStateM (Maybe Entity)
handleWorldAction = \case
    WorldAction_SpawnEntity s -> spawnEntity s
    where
    spawnEntity = \case
        SpawnEntity_Item i -> spawnItem i

    spawnItem i = do
        -- mit <- lookupItemType itName
        let mit = Just (i^.itemType)
        flip (maybe (pure Nothing)) mit $ \it -> do
            let e = toEntity $ makeItem it
                  & location .~ (Just $ i^.location)
            return $ Just e

addDirectedAction :: DirectedAction -> Game ()
addDirectedAction a = userState.gameState.actions %= (a:)

actOnEntity :: EntityId -> EntityAction -> Game ()
actOnEntity eid = addDirectedAction . directAtEntity eid

actOnFocusedEntity :: EntityAction -> Game ()
actOnFocusedEntity act = withFocusId $ \fi -> actOnEntity fi act

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

debugRunAnimation :: AnimationKind -> Game ()
debugRunAnimation = actOnFocusedEntity . EntityAction_DebugRunAnimation

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

executeAttack :: Game ()
executeAttack = actOnFocusedEntity EntityAction_ExecuteAttack

