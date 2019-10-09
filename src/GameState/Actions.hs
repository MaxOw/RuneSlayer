module GameState.Actions where

import Delude
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import Types (Game)
import Types.Entity
import Types.GameState
import Types.Entity.PassiveType
import Types.Entity.Common (EntityId, Volume)
import Types.Equipment (EquipmentSlot(..), Equipment)
import Types.ResourceManager (Resources, agentsMap)
import EntityLike (toEntity)
import Entity.Passive (makePassive)
import Entity.Agent (makeAgent)
import Entity.Effect (makeEffect)
import Entity
import GameState.Query
import ResourceManager (lookupPassive)
import Focus

import qualified Equipment
import qualified Messages

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

pickupItem :: EntityId -> Game ()
pickupItem eid = withFocusId $ passItemTo eid

dropItem :: EntityId -> Game ()
dropItem e = actOnEntity e $ EntityAction_SelfPassTo Nothing Nothing

passItemTo :: EntityId -> EntityId -> Game ()
passItemTo e t = actOnEntity e $ EntityAction_SelfPassTo (Just t) Nothing

passItemToSlot :: EntityId -> EntityId -> EquipmentSlot -> Game ()
passItemToSlot e t s = actOnEntity e $ EntityAction_SelfPassTo (Just t) (Just s)

--------------------------------------------------------------------------------

-- filter as many as possible items that fit into given target
filterFitItems :: EntityWithId -> [EntityWithId] -> Game [EntityId]
filterFitItems tgt items = case tgt^.entity.oracleEquipment of
    Just eq -> filterFitAgent eq
    Nothing -> case tgt^?entity.oraclePassiveType.traverse.containerType of
        Just  _ -> filterFitContainer
        Nothing -> return []
    where
    itf = mapMaybe toFit items
    filterFitAgent eq = do
        let (fs, ufs, feq) = fitIntoEquipment eq itf
        mt <- runMaybeT $ do
            eid <- MaybeT $ pure $ Equipment.lookupSlot EquipmentSlot_Backpack feq
            e <- MaybeT $ lookupEntity eid
            forContainer e ufs
        case mt of
            Nothing          -> returnFit fs ufs
            Just (cfs, cufs) -> returnFit (fs<>cfs) cufs

    filterFitContainer = runMaybeT (forContainer tgt itf) >>= \case
        Nothing        -> return []
        Just (is, ufs) -> returnFit is ufs

    forContainer e is = do
        fv <- MaybeT $ calcFreeVolume e
        print fv
        pc <- MaybeT $ pure $ e^.entity.oraclePassiveType
        ak <- MaybeT $ pure $ pc^?containerType.traverse.ff#allowKinds
        mapM_ print is
        return $ fitIntoContainer (TargetContainer fv ak) is

    returnFit fs (not . null -> leftover) = do
        when leftover $ Messages.addInfo "Not enough space for some items."
        return fs

    toFit x = ItemToFit
        <$> pure (x^.entityId)
        <*> x^.entity.oracleVolume
        <*> x^.entity.oracleFittingSlots
        <*> x^?entity.oraclePassiveType.traverse.ff#passiveKind

data ItemToFit = ItemToFit
   { field_entityId     :: EntityId
   , field_volume       :: Volume
   , field_fittingSlots :: Set EquipmentSlot
   , field_passiveKind  :: Set PassiveKind
   } deriving (Generic, Show)
instance HasEntityId ItemToFit EntityId

data TargetContainer = TargetContainer
   { field_freeVolume   :: Volume
   , field_allowKind    :: Set PassiveKind
   } deriving (Generic)

fitIntoContainer :: TargetContainer -> [ItemToFit] -> ([EntityId], [ItemToFit])
fitIntoContainer tc is = (af^..traverse.entityId, da<>aa)
    where
    (af, aa) = fitVolume al
    (al, da) = splitAllowed is

    splitAllowed
        = List.partition
        $ not . Set.disjoint (tc^.ff#allowKind)
        . view (ff#passiveKind)

    fitVolume = go (tc^.ff#freeVolume) [] []
        where
        go _ xs uf [] = (xs, uf)
        go v xs uf (f:fs)
            | vv < 0    = go v     xs (f:uf) fs
            | otherwise = go vv (f:xs)   uf  fs
            where
            vv = v - f^.ff#volume

data TargetAgent = TargetAgent
   { field_entityId     :: EntityId
   , field_equipment    :: Equipment
   } deriving (Generic)

fitIntoEquipment
    :: Equipment -> [ItemToFit] -> ([EntityId], [ItemToFit], Equipment)
fitIntoEquipment ieq = go ieq [] []
    where
    go eq xs nf [] = (xs, nf, eq)
    go eq xs nf (f:fs) = case tryFitItem f eq of
        Just  s -> go (Equipment.insert s e eq) (e:xs) nf fs where e = f^.entityId
        Nothing -> go eq xs (f:nf) fs

    tryFitItem f eq = viaNonEmpty head $ Set.toList
        $ Set.intersection (f^.fittingSlots) (Equipment.emptySlots eq)

