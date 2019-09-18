module GameState
    ( module Types.GameState
    , zoomGameState
    , updateGameState
    , actOnEntity
    , actOnFocusedEntity
    , actOnPlayer
    , actOnWorld
    , getGameOverScreen

    , isDebugFlagOn
    , pickupItem, dropItem
    , passItemTo, passItemToSlot
    ) where

import Delude
import qualified Data.Set as Set

import Engine (EngineState, userState)
import Types (Game, GameWire(..), St)
import Types.Entity (Entity)
import Types.Entity.Common (EntityId, defaultDelta)
import Types.GameState
import Types.Debug (DebugFlag(..))
import Types.EntityAction
import Types.DirectedAction
import Types.Equipment
import Focus

import InputState.Actions (inspectContent, showStoryDialog)
import GameState.Actions

import qualified Tutorial
import qualified Messages
import qualified EntityIndex

--------------------------------------------------------------------------------

zoomGameState :: GameStateM a -> Game a
zoomGameState = zoom (userState.gameState)

updateGameState :: Game ()
updateGameState = do
    join $ EntityIndex.update
        <$> use (userState.resources)
        <*> pure handleWorldAction
        <*> pure handleEntityActions
        <*> use (gs actions)
        <*> use (gs frameCount)
        <*> use (gs entities)

    gs actions .= []

    Tutorial.update
    Messages.update
    where
    gs l = userState.gameState.l

handleEntityActions :: [DirectedEntityAction] -> Game ()
handleEntityActions as = do
    Tutorial.entityActionsHook as

handleWorldAction :: WorldAction -> Game (Maybe (Entity, SpawnEntityOpts))
handleWorldAction = \case
    WorldAction_SpawnEntity s opts -> fmap (,opts) <$> spawnEntity s
    WorldAction_InspectContent tid -> nop $ inspectContent tid
    WorldAction_StoryDialog eid sd -> nop $ showStoryDialog eid sd
    WorldAction_Message        msg -> nop $ Messages.add msg
    WorldAction_GameOver           -> nop $ startGameOver
    where
    nop x = x >> return Nothing

spawnEntity :: SpawnEntity -> Game (Maybe Entity)
spawnEntity se = do
    rs <- use $ userState.resources
    return $ fromSpawnEntity rs se

startGameOver :: Game ()
startGameOver = do
    screen .= Just def
    userState.ff#wires %= (gameOverWire:)
    where
    gameOverWire = GameWire $ use screen >>= \case
        Nothing -> return Nothing
        Just sc -> if sc^.timer >= 1
            then do
                screen.traverse.ff#pressAnyKey .= True
                return Nothing
            else do
                screen.traverse.timer += (defaultDelta / 2)
                return (Just gameOverWire)

    screen :: Lens' (EngineState St) (Maybe GameOverScreen)
    screen = userState.gameState.ff#gameOverScreen

getGameOverScreen :: Game (Maybe GameOverScreen)
getGameOverScreen = use $ userState.gameState.ff#gameOverScreen

--------------------------------------------------------------------------------

isDebugFlagOn :: DebugFlag -> Game Bool
isDebugFlagOn x = uses (userState.debugFlags) (Set.member x)

pickupItem :: EntityId -> Game ()
pickupItem eid = withFocusId $ passItemTo eid

dropItem :: EntityId -> Game ()
dropItem e = actOnEntity e $ EntityAction_SelfPassTo Nothing Nothing

passItemTo :: EntityId -> EntityId -> Game ()
passItemTo e t = actOnEntity e $ EntityAction_SelfPassTo (Just t) Nothing

passItemToSlot :: EntityId -> EntityId -> EquipmentSlot -> Game ()
passItemToSlot e t s = actOnEntity e $ EntityAction_SelfPassTo (Just t) (Just s)

