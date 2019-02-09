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
    , queryIndex
    ) where

import Delude
import Data.List (partition)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import Text.Printf
import Types.Entity
import Types.Entity.Common (EntityId (..))
import Engine.KDTree (buildKDTree)
import qualified Engine.KDTree as KDTree
-- import Engine.Common.Types (BBox)

--------------------------------------------------------------------------------

emptyIndex :: EntityIndex
emptyIndex = EntityIndex
   { entityIndex_lastId              = EntityId 0
   , entityIndex_entities            = HashMap.empty
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
    let prt x = putStrLn ((printf "%d: %s" fct (show x :: String)) :: String)
    mapM_ prt $ filter (noSetMoveVector . view action) directedActions
    -- mapM_ prt directedActions
    return $ eix
        & entities       .~ HashMap.union changedMap toLeaveMap
        & staticIndex    .~ HashSet.difference newStaticSet    toDeleteSet
        & dynamicIndex   .~ HashSet.difference newDynamicSet   toDeleteSet
        & activatedIndex .~ newActivatedSet
    -- , entityIndexLocation = rebuildLocationIndex actedOnEntities
    where

    -- Set of entity ids to process in this update O(n+m)
    toProcSet  = HashSet.union (eix^.activatedIndex) (eix^.dynamicIndex)
    -- Map of entities to process O(n*log m)
    toProcMap  = HashMap.intersection (eix^.entities) (HashSet.toMap toProcSet)
    -- Map of entities not in need of processing O(n*log m)
    toLeaveMap = HashMap.difference   (eix^.entities) (HashSet.toMap toProcSet)

    -- List of result after applying update function on each entity toProc O(n)
    updateResult :: [(EntityId, (Maybe Entity, [DirectedEntityAction]))]
    updateResult = map (uncurry updateEntity) $ HashMap.toList toProcMap

    -- Set of entities to delete from all indexes
    toDeleteSet = HashSet.fromList
        $ map fst $ filter (isNothing . fst . snd) updateResult

    -- Map of entities that were updated and didn't die in the process
    onlyUpdatedMap = HashMap.fromList
        $ mapMaybe (\(i,(me,_)) -> (i,) <$> me) updateResult

    -- Map of all entities after update
    updatedMap = HashMap.union onlyUpdatedMap toLeaveMap
    -- List of directed actions resulting from update + global ones
    directedActions = concatMap (snd.snd) updateResult <> globalActions
    -- Set of entities to be acted on in this update step
    toActOnSet = HashSet.fromList $ map (view entityId) directedActions
    -- Map of entities to be acted on in this update step
    toActOnMap = HashMap.intersection updatedMap $ HashSet.toMap toActOnSet
    -- Final map of entities after performing all directed actions on them
    actedOnMap = foldr performAction toActOnMap directedActions

    -- Set of static entities that were newly activated in this step
    newActivatedSet = HashSet.intersection toActOnSet (eix^.staticIndex)

    -- Map of all entities that could possibly change it this step
    changedMap = HashMap.union actedOnMap onlyUpdatedMap

    changedStaticMap = HashMap.filter (view (oracle.static)) changedMap
    newStaticSet
        = HashSet.union (hashMap_keysSet changedStaticMap) (eix^.staticIndex)

    changedDynamicMap = HashMap.filter (not . view (oracle.static)) changedMap
    newDynamicSet
        = HashSet.union (hashMap_keysSet changedDynamicMap) (eix^.dynamicIndex)

    hashMap_keysSet x = HashSet.fromMap (() <$ x)

    updateEntity k v = (k, entityUpdate v ctx)
        where
        ctx = EntityContext
            { entityContext_entities   = eix
            , entityContext_selfId     = k
            , entityContext_frameCount = fct
            }

    performAction (DirectedEntityAction eid act)
        = HashMap.adjust (flip entityActOn act) eid

{-
    h = HashMap.mapWithKey updateEntity $ view entities eix
    directedActions = concatMap snd (HashMap.elems h) <> globalActions
    updatedEntities = HashMap.mapMaybe fst h
    actedOnEntities = foldr performAction updatedEntities directedActions
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
getLastId eix
    | eid <= EntityId 0 = Nothing
    | otherwise         = Just eid
    where eid = eix^.lastId

addToIndex :: Entity -> EntityIndex -> EntityIndex
addToIndex e eix = eix
    & lastId   .~ newId
    & entities .~ newMap
 -- , entityIndexLocation = addLocationIndex (queryLocation e) newId
    where
    newId  = EntityId $ unEntityId (eix^.lastId) + 1
    newMap = HashMap.insert newId e $ eix^.entities

lookupEntityById :: EntityId -> EntityIndex -> Maybe Entity
lookupEntityById eid eix = HashMap.lookup eid $ view entities eix

-- TODO: For not this just returs list of all entities but in the future we
-- should be quering for only visible entities within ViewRange from a space
-- partitioning tree of entities.
-- type ViewRange = BBox Float
entitiesInRange :: ViewRange -> EntityIndex -> [EntityWithId]
entitiesInRange r e
    = dynamicEntitiesInRange r e
    <> staticEntitiesInRange r e

staticEntitiesInRange :: ViewRange -> EntityIndex -> [EntityWithId]
staticEntitiesInRange r e =
    selectEntities (KDTree.lookup r $ e^.staticLocationIndex) e
 -- selectEntities (e^.staticIndex) e
    where
    -- er = expandRange r maxEntitySize
    -- maxEntitySize = 3 -- meters

dynamicEntitiesInRange :: ViewRange -> EntityIndex -> [EntityWithId]
dynamicEntitiesInRange _r e = selectEntities (e^.dynamicIndex) e
    where
    -- er = expandRange r maxEntitySize
    -- maxEntitySize = 3 -- meters

-- TODO: this again is a naive filtering of all entities without emploing any
-- sort of index what so ever. Obviously this needs to be improved in the future
-- once it will become a bottleneck.
queryIndex :: (EntityOracle -> Bool) -> EntityIndex -> [EntityWithId]
queryIndex f = filter (f . view (entity.oracle)) . allEntities

allEntities :: EntityIndex -> [EntityWithId]
allEntities = map (uncurry EntityWithId) . HashMap.toList . view entities

selectEntities :: Foldable t => t EntityId -> EntityIndex -> [EntityWithId]
selectEntities s e
    = mapMaybe (\k -> EntityWithId k <$> HashMap.lookup k (e^.entities))
    $ toList s


