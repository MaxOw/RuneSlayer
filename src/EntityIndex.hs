module EntityIndex
    ( EntityIndex

    , new
    , update
    , insert
    , lookupInRange
    , lookupById
    , lookupManyById
    ) where

import Delude
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Text.Printf

import Engine.Common.Types (minPoint, maxPoint)
import Types.Entity
import Types.Entity.Common (EntityId (..), EntityKind (..))
import Data.VectorIndex (VectorIndex)
import qualified Data.VectorIndex as VectorIndex
import qualified Data.SpatialIndex as SpatialIndex
import qualified Data.FullMap as FullMap
import Types.ResourceManager (Resources)

--------------------------------------------------------------------------------

new :: MonadIO m => EntityIndexConfig -> m EntityIndex
new conf = do
    let maxSizeDim = fromMaybe 1 $ maximumOf traverse $ conf^.size
    v <- VectorIndex.new
    let g = def
          & ff#gridSize .~ (floor <$> conf^.size)
          & ff#cellSize .~ pure 2
    let qt = def
          & size          .~ maxSizeDim
          & minCellSize   .~ 2
          & maxBucketSize .~ 4
    sTile    <- SpatialIndex.createGrid g
    sItem    <- SpatialIndex.createQuadTree qt
    sStatic  <- SpatialIndex.createQuadTree qt
    sDynamic <- SpatialIndex.createQuadTree qt
    let f = \case
            EntityKind_Tile    -> sTile
            EntityKind_Item    -> sItem
            EntityKind_Dynamic -> sDynamic
            EntityKind_Static  -> sStatic
    lRef <- newIORef Nothing
    dRef <- newIORef mempty
    aRef <- newIORef mempty
    return $ EntityIndex
        { field_lastId              = lRef
        , field_entities            = v

        , field_dynamicIndex        = dRef
        , field_activatedList       = aRef
        , field_spatialIndex        = FullMap.build f
        }

noSetMoveVector :: EntityAction -> Bool
noSetMoveVector (EntityAction_SetMoveVector {}) = False
noSetMoveVector _                               = True

update :: MonadIO m
    => Resources
    -> (WorldAction -> m (Maybe Entity))
    -> [DirectedAction] -> Word32 -> EntityIndex -> m ()
update res handleWorldAction globalActions fct eix = do

    actList <- readIORef $ eix^.activatedList
    dynSet  <- readIORef $ eix^.dynamicIndex
    -- List of entity ids to process in this update O(n+m)
    let toProcIdList = actList <> toList dynSet
    -- List of entities in need of processing O(n)
    toProcList <- liftIO $ lookupManyById toProcIdList eix

    -- List of results after applying update function on each entity toProc O(n)
    -- [(EntityId, (Maybe Entity, [DirectedEntityAction]))]
    updateResult <- forM toProcList $ \(EntityWithId k v) ->
        let ctx = EntityContext
                { field_entities   = eix
                , field_selfId     = k
                , field_frameCount = fct
                , field_resources  = res }
        in (k,) <$> liftIO (runQ (entityUpdate v ctx))

    -- List of directed actions resulting from update + global ones
    let directedActions = concatMap (snd.snd) updateResult <> globalActions
    let (actionsAtEntity, actionsAtWorld) = partitionActions directedActions
    debugPrintActions actionsAtEntity

    -- Map of entity action grouped by target entity id
    let directedActionsMap :: HashMap EntityId [EntityAction]
        directedActionsMap = HashMap.fromListWith (<>) $ map f actionsAtEntity
            where f d = (d^.entityId, [d^.action])

    -- Set of all entity ids that changed in this frame
    let changedList = hashNub $
            (map fst updateResult) <> (map (view entityId) actionsAtEntity)
    -- List of values that entities had before change
    preChangeList <- liftIO $ lookupManyById changedList eix

    -- Write updated entities to index
    forM_ updateResult $ \(i, (me,_)) -> do
        VectorIndex.update (i^.offset) (EntityWithId i <$> me) (eix^.entities)

    --  (EntityId, [EntityAction]) -> Q (Maybe (Int, EntityWithId))
    let performActions (eid, as) = fmap f <$> liftIO (lookupById eid eix)
            where f = (eid^.offset,) . over entity (flip (foldl' entityActOn) as)

    -- Result of directed actions
    -- [(Int, Maybe EntityWithId)]
    actOnResult <- catMaybes
        <$> mapM performActions (HashMap.toList directedActionsMap)

    -- Write acted on entities to index
    forM_ actOnResult $ \(i, e) -> VectorIndex.update i (Just e) (eix^.entities)

    -- Preform world actions
    toAddToIndexList <- map (\e -> (e^.entityId, Nothing, Just $ e^.entity))
         . catMaybes <$> mapM handleAndInsert actionsAtWorld

    -- Reindex changed entities
    toUpdateOnIndexList <- forM preChangeList $ \ewid -> do
        let i = ewid^.entityId
        newEwid <- liftIO $ lookupById i eix
        return (i, Just $ ewid^.entity, view entity <$> newEwid)
    let toReindexList = toUpdateOnIndexList <> toAddToIndexList
    mapM_ (reindex eix) toReindexList

    -- Update list of activated entities to be processed in next frame
    directedList <- liftIO $ lookupManyById (HashMap.keys directedActionsMap) eix
    let f = filter (\e -> entityKind (e^.entity) /= EntityKind_Dynamic)
    writeIORef (eix^.activatedList) (map (view entityId) $ f directedList)

    -- Update index of dynamic entities
    let dynDeleteSet = makeDynSet $ filter shouldDeleteDyn toReindexList
    let dynInsertSet = makeDynSet $ filter shouldInsertDyn toReindexList
    modifyIORef (eix^.dynamicIndex) $ \s ->
        HashSet.difference (HashSet.union s dynInsertSet) dynDeleteSet

    where
    makeDynSet = HashSet.fromList . map (view _1)

    shouldDeleteDyn (_, Nothing, Nothing) = False
    shouldDeleteDyn (_, Nothing, Just _v) = False
    shouldDeleteDyn (_, Just ov, Nothing) = entityKind ov == EntityKind_Dynamic
    shouldDeleteDyn (_, Just ov, Just nv)
        =  entityKind ov == EntityKind_Dynamic
        && entityKind nv /= EntityKind_Dynamic

    shouldInsertDyn (_, Nothing, Nothing) = False
    shouldInsertDyn (_, Nothing, Just nv) = entityKind nv == EntityKind_Dynamic
    shouldInsertDyn (_, Just _v, Nothing) = False
    shouldInsertDyn (_, Just ov, Just nv)
        =  entityKind ov /= EntityKind_Dynamic
        && entityKind nv == EntityKind_Dynamic

    partitionActions = go [] []
        where
        go es ws [] = (es, ws)
        go es ws (x:xs) = case x of
            DirectedAtEntity e -> go (e:es) ws xs
            DirectedAtWorld  w -> go es (w:ws) xs

    handleAndInsert w = do
        me <- handleWorldAction w
        case me of
            Nothing -> return Nothing
            Just en -> do
                i <- insert en eix
                return $ Just $ EntityWithId i en

    action = field' @"field_action"

    ----------------------------------------------------------------------------
    debugPrintActions = mapM_ prt . filter (noSetMoveVector . view action)
    prt x = putStrLn ((printf "%d: %s" fct (show x :: String)) :: String)

reindex :: MonadIO m
    => EntityIndex
    -> (EntityId, Maybe Entity, Maybe Entity)
    -> m ()
reindex eix (i, mo, mn) = case (mo, mn) of
    (Nothing, Nothing) -> return ()
    (Nothing, Just nv) -> addOnIndex nv
    (Just ov, Nothing) -> deleteFromIndex ov
    (Just ov, Just nv) -> updateInIndex ov nv
    where
    addOnIndex nv = do
        let ki = FullMap.lookup (entityKind nv) (eix^.spatialIndex)
        let pos = getPosM nv
        whenJust pos $ \p -> SpatialIndex.insert p i ki

    deleteFromIndex ov = do
        let ki = FullMap.lookup (entityKind ov) (eix^.spatialIndex)
        let pos = getPosM ov
        whenJust pos $ \p -> SpatialIndex.delete p i ki

    updateInIndex ov nv = do
        let ko = entityKind ov
        let kn = entityKind nv
        if ko /= kn
        then deleteFromIndex ov >> addOnIndex nv
        else do
            let ki = FullMap.lookup kn (eix^.spatialIndex)
            let oPos = getPosM ov
            let nPos = getPosM nv
            case (oPos, nPos) of
                (Nothing, Nothing) -> return ()
                (Nothing, Just _P) -> addOnIndex nv
                (Just _P, Nothing) -> deleteFromIndex ov
                (Just oP, Just nP) -> SpatialIndex.move oP nP i ki

    getPosM x = x^?oracle.location.traverse._Wrapped

insert :: MonadIO m => Entity -> EntityIndex -> m EntityId
insert e eix = do
    old <- readIORef (eix^.lastId)
    let newUnique = maybe 0 (\x -> x^.unique+1) old
    let mkId = EntityId newUnique
    o <- VectorIndex.insertWithKey (flip EntityWithId e . mkId) (eix^.entities)
    let newId = mkId o
    reindex eix (newId, Nothing, Just e)
    when (entityKind e == EntityKind_Dynamic) $
        modifyIORef (eix^.dynamicIndex) $ HashSet.insert newId
    writeIORef (eix^.lastId) (Just newId)
    return newId

lookupById :: MonadQ m => EntityId -> EntityIndex -> m (Maybe EntityWithId)
lookupById eid = liftQ . lookupVector eid . view entities

lookupVector :: EntityId -> VectorIndex EntityWithId -> Q (Maybe EntityWithId)
lookupVector eid v = Q (VectorIndex.lookup (eid^.offset) v) >>= \case
    Just x | x^.entityId.unique == eid^.unique -> return $ Just x
    _ -> return Nothing

lookupInRange :: MonadQ m
    => EntityKind -> RangeBBox -> EntityIndex -> m [EntityWithId]
lookupInRange k r eix = do
    let ki = FullMap.lookup k (eix^.spatialIndex)
    let er = expandRange expandSizeByKind r
    eis <- liftQ $ Q $ SpatialIndex.lookup er ki
    lookupManyById eis eix
    where
    expandSizeByKind = case k of
        EntityKind_Tile    -> 0.5
        EntityKind_Static  -> 4
        EntityKind_Item    -> 0.5
        EntityKind_Dynamic -> 2

    -- expandRange :: Num x => x -> BBox x -> BBox x
    expandRange e bb = bb
        & minPoint %~ fmap (\x -> x-e)
        & maxPoint %~ fmap (\x -> x+e)

lookupManyById :: (Foldable t, MonadQ m)
    => t EntityId -> EntityIndex -> m [EntityWithId]
lookupManyById s e = catMaybes <$> mapM (flip lookupById e) (toList s)

