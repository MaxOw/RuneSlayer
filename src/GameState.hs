module GameState
    ( module Types.GameState
    , zoomGameState
    , updateGameState
    , actOnEntity
    , actOnFocusedEntity
    , actOnPlayer
    , getGameOverScreen

    , isDebugFlagOn
    , pickupItem, dropItem
    , passItemTo, passItemToSlot
    ) where

import Delude
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap

import Engine (EngineState, userState)
import Types (Game, GameWire(..), St)
import Types.Entity (Entity)
import Types.Entity.Common (EntityId, defaultDelta)
import Types.GameState
import Types.Debug (DebugFlag(..))
import Types.EntityAction
import Types.DirectedAction
import Types.Entity.Agent
import Types.Equipment
import Focus

import EntityLike (toEntity)
import Entity.Passive (makePassive)
import Entity.Agent (makeAgent)
import Entity.Effect (makeEffect)
import ResourceManager (lookupPassive)
import Types.ResourceManager (agentsMap)
import InputState.Actions (inspectContent, showStoryDialog)

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
    spawnEntity = \case
        SpawnEntity_Passive    n -> spawnPassive n
        SpawnEntity_Agent      n -> spawnAgent n

        SpawnEntity_Effect   l s -> spawnEffect l s
        SpawnEntity_Projectile p -> spawnProjectile p

    spawnPassive n = do
        rs  <- use $ userState.resources
        let mit = lookupPassive n rs
        flip (maybe (pure Nothing)) mit $ \it -> do
            let e = toEntity $ makePassive rs it
            return $ Just e

    nop x = x >> return Nothing

    spawnAgent n = do
        mit <- lookupAgentType n
        rs  <- use $ userState.resources
        flip (maybe (pure Nothing)) mit $ \it -> do
            let e = toEntity $ makeAgent rs it
            return $ Just e

    spawnEffect l s = do
        return $ Just $ toEntity $ makeEffect s & location .~ l

    spawnProjectile = return . Just . toEntity

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

lookupAgentType :: AgentTypeName -> Game (Maybe AgentType)
lookupAgentType n = do
    rs <- use $ userState.resources.agentsMap
    return $ HashMap.lookup n rs

addDirectedAction :: DirectedAction -> Game ()
addDirectedAction a = userState.gameState.actions %= (a:)

actOnEntity :: HasEntityId e EntityId => e -> EntityAction -> Game ()
actOnEntity (view entityId -> eid) = addDirectedAction . directAtEntity eid

actOnFocusedEntity :: EntityAction -> Game ()
actOnFocusedEntity act = withFocusId $ \fi -> actOnEntity fi act

actOnPlayer :: PlayerAction -> Game ()
actOnPlayer = actOnFocusedEntity . EntityAction_PlayerAction

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

