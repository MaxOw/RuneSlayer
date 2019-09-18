module GameState.Actions where

import Delude
import qualified Data.HashMap.Strict as HashMap
import Types (Game)
import Types.Entity
import Types.GameState
import Types.Entity.Common (EntityId)
import Types.ResourceManager (Resources, agentsMap)
import EntityLike (toEntity)
import Entity.Passive (makePassive)
import Entity.Agent (makeAgent)
import Entity.Effect (makeEffect)
import ResourceManager (lookupPassive)
import Focus

--------------------------------------------------------------------------------

addDirectedAction :: DirectedAction -> Game ()
addDirectedAction a = gameState.actions %= (a:)

actOnEntity :: HasEntityId e EntityId => e -> EntityAction -> Game ()
actOnEntity (view entityId -> eid) = addDirectedAction . directAtEntity eid

actOnFocusedEntity :: EntityAction -> Game ()
actOnFocusedEntity act = withFocusId $ \fi -> actOnEntity fi act

actOnPlayer :: PlayerAction -> Game ()
actOnPlayer = actOnFocusedEntity . EntityAction_PlayerAction

actOnWorld :: WorldAction -> Game ()
actOnWorld = addDirectedAction . directAtWorld

fromSpawnEntity :: Resources -> SpawnEntity -> Maybe Entity
fromSpawnEntity rs = \case
    SpawnEntity_Passive    n -> spawnPassive n
    SpawnEntity_Agent      n -> spawnAgent n

    SpawnEntity_Effect     s -> spawnEffect s
    SpawnEntity_Projectile p -> spawnProjectile p
    where
    spawnPassive  n = toEntity . makePassive rs <$> lookupPassive n rs
    spawnAgent    n = toEntity . makeAgent rs <$> HashMap.lookup n (rs^.agentsMap)
    spawnEffect     = Just . toEntity . makeEffect
    spawnProjectile = Just . toEntity

