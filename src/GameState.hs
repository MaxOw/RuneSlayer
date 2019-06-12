module GameState
    ( module Types.GameState
    , zoomGameState
    , updateGameState
    , actOnEntity
    , actOnFocusedEntity
    , actOnPlayer

    , isDebugFlagOn
    , pickupItem, dropItem
    ) where

import Delude
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap

import Engine (userState)
import Types (Game)
import Types.Entity (Entity)
import Types.Entity.Common (EntityId)
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

handleWorldAction :: WorldAction -> Game (Maybe Entity)
handleWorldAction = \case
    WorldAction_SpawnEntity s -> spawnEntity s
    where
    spawnEntity = \case
        SpawnEntity_Item       s -> spawnItem s
        SpawnEntity_Unit       s -> spawnUnit s
        SpawnEntity_Effect   l s -> spawnEffect l s
        SpawnEntity_Projectile p -> spawnProjectile p

    spawnItem s = do
        mit <- lookupItemType $ s^.name
        flip (maybe (pure Nothing)) mit $ \it -> do
            let e = toEntity $ makeItem it
                  & location .~ (Just $ s^.location)
            return $ Just e

    spawnUnit s = do
        mit <- lookupUnitType $ s^.name
        rs  <- use $ userState.resources
        flip (maybe (pure Nothing)) mit $ \it -> do
            let e = toEntity $ makeUnit rs it
                  & location .~ (s^.location)
            return $ Just e

    spawnEffect l s = do
        return $ Just $ toEntity $ makeEffect s & location .~ l

    spawnProjectile = return . Just . toEntity

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

--------------------------------------------------------------------------------

isDebugFlagOn :: DebugFlag -> Game Bool
isDebugFlagOn x = uses (userState.debugFlags) (Set.member x)

pickupItem :: EntityId -> Game ()
pickupItem eid = withFocusId $ actOnEntity eid . EntityAction_SelfAddedBy

dropItem :: EntityId -> Game ()
dropItem = actOnFocusedEntity . EntityAction_DropItem

