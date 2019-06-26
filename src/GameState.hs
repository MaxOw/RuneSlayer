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
import Types.Entity.Item
import Types.Entity.Unit
import Focus

import EntityLike (toEntity)
import Entity.Item (makeItem)
import Entity.Unit (makeUnit)
import Entity.Effect (makeEffect)

import qualified EntityIndex

--------------------------------------------------------------------------------

zoomGameState :: GameStateM a -> Game a
zoomGameState = zoom (userState.gameState)

updateGameState :: Game ()
updateGameState = do
    join $ EntityIndex.update
        <$> use (userState.resources)
        <*> pure handleWorldAction
        <*> use (gs actions)
        <*> use (gs frameCount)
        <*> use (gs entities)

    gs actions .= []

    where
    gs l = userState.gameState.l

handleWorldAction :: WorldAction -> Game (Maybe (Entity, SpawnEntityOpts))
handleWorldAction = \case
    WorldAction_SpawnEntity s opts -> fmap (,opts) <$> spawnEntity s
    WorldAction_GameOver -> startGameOver >> return Nothing
    where
    spawnEntity = \case
        SpawnEntity_Item       n -> spawnItem n
        SpawnEntity_Unit       n -> spawnUnit n
        SpawnEntity_Effect   l s -> spawnEffect l s
        SpawnEntity_Projectile p -> spawnProjectile p

    spawnItem n = do
        mit <- lookupItemType n
        rs  <- use $ userState.resources
        flip (maybe (pure Nothing)) mit $ \it -> do
            let e = toEntity $ makeItem rs it
            return $ Just e

    spawnUnit n = do
        mit <- lookupUnitType n
        rs  <- use $ userState.resources
        flip (maybe (pure Nothing)) mit $ \it -> do
            let e = toEntity $ makeUnit rs it
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

lookupItemType :: ItemTypeName -> Game (Maybe ItemType)
lookupItemType n = do
    rs <- use $ userState.resources.itemsMap
    return $ HashMap.lookup n rs

lookupUnitType :: UnitTypeName -> Game (Maybe UnitType)
lookupUnitType n = do
    rs <- use $ userState.resources.unitsMap
    return $ HashMap.lookup n rs

addDirectedAction :: DirectedAction -> Game ()
addDirectedAction a = userState.gameState.actions %= (a:)

actOnEntity :: EntityId -> EntityAction -> Game ()
actOnEntity eid = addDirectedAction . directAtEntity eid

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
pickupItem eid = withFocusId $ actOnEntity eid . EntityAction_SelfAddedBy

dropItem :: EntityId -> Game ()
dropItem = actOnFocusedEntity . EntityAction_DropItem

