module EntityIndex
    ( EntityIndex

    , emptyIndex
    , updateIndex
    , addToIndex
    , rebuildIndex
    , getLastId
    , entitiesInRange
    , staticEntitiesInRange
    , dynamicEntitiesInRange
    , lookupEntityById
    ) where

import Delude
import Data.List (partition)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Text.Printf
import Types.Entity
import Types.Entity.Common (EntityId (..))
import Engine.KDTree (buildKDTree)
import qualified Engine.KDTree as KDTree
import Engine.Common.Types

--------------------------------------------------------------------------------

emptyIndex :: EntityIndex
emptyIndex = EntityIndex
   { entityIndex_lastId              = Nothing
   , entityIndex_emptyOffsets        = []
   , entityIndex_entities            = Vector.empty
   , entityIndex_staticIndex         = mempty
   , entityIndex_dynamicIndex        = mempty
   , entityIndex_activatedIndex      = mempty
   , entityIndex_staticLocationIndex = buildKDTree mempty
   }

noSetMoveVector :: EntityAction -> Bool
noSetMoveVector (EntityAction_SetMoveVector {}) = False
noSetMoveVector _                               = True

updateIndex
    :: MonadIO m
    => [DirectedEntityAction] -> Word32 -> EntityIndex -> m EntityIndex
updateIndex globalActions fct eix = do
    debugPrintDirectedActions
    return $ eix
        & entities       .~ actedOnEntities
        & activatedIndex .~ newActivatedSet
    where

    -- Set of entity ids to process in this update O(n+m)
    toProcSet  = HashSet.union (eix^.activatedIndex) (eix^.dynamicIndex)
    -- List of entities in need of processing O(n)
    toProcList = selectEntities toProcSet eix

    -- List of results after applying update function on each entity toProc O(n)
    updateResult :: [(EntityId, (Maybe Entity, [DirectedEntityAction]))]
    updateResult = map updateEntity toProcList
    updateEntity (EntityWithId k v) = (k, entityUpdate v ctx)
        where
        ctx = EntityContext
            { entityContext_entities   = eix
            , entityContext_selfId     = k
            , entityContext_frameCount = fct
            }

    -- Vector of all entities after update O(k)
    updatedEntities :: Vector (Maybe EntityWithId)
    updatedEntities = updateEntitiesVector (map f updateResult) (eix^.entities)
        where f (i,(me,_)) = (i,me)

    -- List of directed actions resulting from update + global ones
    directedActions = concatMap (snd.snd) updateResult <> globalActions

    -- Map of entity action grouped by target entity id
    directedActionsMap :: HashMap EntityId [EntityAction]
    directedActionsMap = HashMap.fromListWith (<>) $ map f directedActions
        where
        f d = (d^.entityId, [d^.action])

    performActions :: (EntityId, [EntityAction]) -> Maybe EntityWithId
    performActions (eid, as) = fmap f $ lookupVector eid updatedEntities
        where f = over entity (flip (foldl' entityActOn) as)

    actOnResult :: [(Int, Maybe EntityWithId)]
    actOnResult = mapMaybe (\p -> (p^._1.offset,) . Just <$> performActions p)
                $ HashMap.toList directedActionsMap

    -- Vector of all entities after update and directed actions O(k)
    actedOnEntities :: Vector (Maybe EntityWithId)
    actedOnEntities
        = Vector.update updatedEntities $ Vector.fromList actOnResult

    -- Set of static entities that were newly activated in this step
    newActivatedSet
        = HashSet.fromList
        $ map (view entityId) . filter (view (entity.oracle.static))
        $ selectEntities (HashMap.keys directedActionsMap) eix

    ----------------------------------------------------------------------------
    debugPrintDirectedActions
        = mapM_ prt $ filter (noSetMoveVector . view action) directedActions
    prt x = putStrLn ((printf "%d: %s" fct (show x :: String)) :: String)



{-
    -- Set of entities to delete from all indexes
    toDeleteSet = HashSet.fromList
        $ map fst $ filter (isNothing . fst . snd) updateResult

    changedStaticMap = HashMap.filter (view (oracle.static)) changedMap
    newStaticSet
        = HashSet.union (hashMap_keysSet changedStaticMap) (eix^.staticIndex)

    changedDynamicMap = HashMap.filter (not . view (oracle.static)) changedMap
    newDynamicSet
        = HashSet.union (hashMap_keysSet changedDynamicMap) (eix^.dynamicIndex)
-}

rebuildIndex :: EntityIndex -> EntityIndex
rebuildIndex eix = eix
    & staticIndex         .~ (HashSet.fromList $ allStatic ^..traverse.entityId)
    & dynamicIndex        .~ (HashSet.fromList $ allDynamic^..traverse.entityId)
    & staticLocationIndex .~ buildKDTree statLocs
    where
    (allStatic, allDynamic) = partition isStatic $ allEntities eix
    isStatic = view (entity.oracle.static)
    toKDEntry e = (, e^.entityId) . fmap realToFrac <$> fromLoc e
    fromLoc e = e^?entity.oracle.location.traverse._Wrapped
    statLocs = Vector.fromList $ mapMaybe toKDEntry allStatic

getLastId :: EntityIndex -> Maybe EntityId
getLastId = view lastId

addToIndex :: Entity -> EntityIndex -> EntityIndex
addToIndex e eix = eix
    & lastId       .~ Just newId
    & emptyOffsets %~ drop 1
    & entities     .~ newVec
 -- , entityIndexLocation = addLocationIndex (queryLocation e) newId
    where
    newId     = EntityId newCount vecOffset
    newCount  = maybe 0 (\(EntityId c _) -> c+1) (eix^.lastId)
    eos       = eix^.emptyOffsets
    vec       = eix^.entities
    vecOffset = fromMaybe (Vector.length vec) (viaNonEmpty head eos)
    newEid    = EntityWithId newId e
    newVec    = case viaNonEmpty head eos of
        Nothing -> Vector.snoc   vec $ Just newEid
        Just vo -> Vector.update vec $ Vector.singleton (vo, Just newEid)

lookupEntityById :: EntityId -> EntityIndex -> Maybe EntityWithId
lookupEntityById eid = lookupVector eid . view entities

lookupVector :: EntityId -> Vector (Maybe EntityWithId) -> Maybe EntityWithId
lookupVector eid v = case join $ Vector.indexM v (eid^.offset) of
    Just x | x^.entityId.unique == eid^.unique -> Just x
    _ -> Nothing

entitiesInRange :: ViewRange -> EntityIndex -> [EntityWithId]
entitiesInRange r e
    = dynamicEntitiesInRange r e
    <> staticEntitiesInRange r e

staticEntitiesInRange :: ViewRange -> EntityIndex -> [EntityWithId]
staticEntitiesInRange r e =
    selectEntities (KDTree.lookup er $ e^.staticLocationIndex) e
    where
    er = expandRange maxEntitySize r

dynamicEntitiesInRange :: ViewRange -> EntityIndex -> [EntityWithId]
dynamicEntitiesInRange _r e = selectEntities (e^.dynamicIndex) e
    where
    -- er = expandRange maxEntitySize r

expandRange :: Num x => x -> BBox x -> BBox x
expandRange e bb = bb
    & minPoint %~ fmap (\x -> x-e)
    & maxPoint %~ fmap (\x -> x+e)

maxEntitySize :: Float
maxEntitySize = 2 -- meters

-- TODO: this again is a naive filtering of all entities without emploing any
-- sort of index what so ever. Obviously this needs to be improved in the future
-- once it will become a bottleneck.
-- queryIndex :: (EntityOracle -> Bool) -> EntityIndex -> [EntityWithId]
-- queryIndex f = filter (f . view (entity.oracle)) . allEntities

allEntities :: EntityIndex -> [EntityWithId]
allEntities = catMaybes . toList . view entities

selectEntities :: Foldable t => t EntityId -> EntityIndex -> [EntityWithId]
selectEntities s e = mapMaybe (flip lookupEntityById e) $ toList s

{-
selectVector :: Foldable t
    => t EntityId -> Vector (Maybe EntityWithId) -> [EntityWithId]
selectVector s e = mapMaybe (flip lookupVector e) $ toList s
-}

updateEntitiesVector
    :: [(EntityId, Maybe Entity)]
    -> Vector (Maybe EntityWithId)
    -> Vector (Maybe EntityWithId)
updateEntitiesVector us v = Vector.update v $ Vector.fromList $ map f us
    where
    f (i, me) = (i^.offset, EntityWithId i <$> me)


