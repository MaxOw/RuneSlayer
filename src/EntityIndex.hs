{-# Language BangPatterns #-}
module EntityIndex
    ( EntityIndex, EntityIndexTag (..)

    , new
    , update
    , insert
    , addTag
    , lookupInRange
    , lookupInRadius
    , lookupById
    , lookupByTag
    , lookupManyById

    , insertStaticShape
    , lookupStaticShapesInRange
    ) where

import Delude
import Data.Time.Clock (getCurrentTime)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict     as Map
import Text.Printf

import Engine.Common.Types (minPoint, maxPoint, mkBBoxCenter)
import Entity
import Types.Entity.Common
    (Location, Distance, EntityId (..) , EntityKind (..), isWithinDistance)
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
    -- let g = def
    let makeG x = def
          & ff#gridSize .~ (floor <$> conf^.size)
          & ff#cellSize .~ pure x
    let qt = def
          & size          .~ maxSizeDim
          & minCellSize   .~ 2
          & maxBucketSize .~ 4
    sTile    <- SpatialIndex.createGrid $ makeG 2
    -- sPassive <- SpatialIndex.createGrid $ makeG 2
    -- sDynamic <- SpatialIndex.createGrid $ makeG 2
    sPassive <- SpatialIndex.createQuadTree qt
    sDynamic <- SpatialIndex.createQuadTree qt
    let f = \case
            EntityKind_Tile    -> sTile
            EntityKind_Passive -> sPassive
            EntityKind_Dynamic -> sDynamic
    sShapes <- SpatialIndex.createGrid $ makeG 2
    lRef <- newIORef Nothing
    dRef <- newIORef mempty
    aRef <- newIORef mempty
    tRef <- newIORef mempty
    return $ EntityIndex
        { field_lastId              = lRef
        , field_entities            = v

        , field_dynamicIndex        = dRef
        , field_activatedSet        = aRef
        , field_spatialIndex        = FullMap.build f
        , field_staticShapes        = sShapes
        , field_tags                = tRef
        }

noSetMoveVector :: EntityAction -> Bool
noSetMoveVector (EntityAction_SetMoveVector {}) = False
noSetMoveVector _                               = True

getDynamicIndex :: MonadIO m => EntityIndex -> m (HashSet EntityId)
getDynamicIndex eix = readIORef $ eix^.dynamicIndex

{-
-- COMMENTED OUT: This probably leaks memory.

-- Only update dynamic entities within some distance from camera
-- This is a brain dead optimization thats good for now but ideally should be
-- replaced by something smarter later.
getDynamicIndex :: MonadIO m => EntityIndex -> m (HashSet EntityId)
getDynamicIndex eix = do
    mc <- liftIO $ lookupByTag EntityIndexTag_Camera eix
    case mc >>= view (entity.oracleLocation) of
        Nothing  -> readIORef $ eix^.dynamicIndex
        Just loc -> do
            let dynamicUpdateDist = distanceInMeters 11
            let dynRange = makeRangeSquare loc dynamicUpdateDist
            let conv = HashSet.fromList . map (view entityId)
            conv <$> liftIO (lookupInRange EntityKind_Dynamic dynRange eix)
-}

update :: MonadIO m
    => Resources
    -> (WorldAction -> m (Maybe (Entity, SpawnEntityOpts)))
    -> ([DirectedEntityAction] -> m ())
    -> [DirectedAction] -> Word32 -> EntityIndex -> m ()
update res handleWorldAction handleEntityActions globalActions fct eix = do

    actSet <- readIORef $ eix^.activatedSet
    dynSet <- getDynamicIndex eix
    -- List of entity ids to process in this update
    let toProcIdList = toList actSet <> toList dynSet
    -- List of entities in need of processing
    toProcList <- liftIO $ lookupManyById toProcIdList eix

    cft <- liftIO getCurrentTime

    -- List of results after applying update function on each entity toProc
    -- [(EntityId, (Maybe Entity, [DirectedEntityAction]))]
    updateResult <- forM toProcList $ \(EntityWithId k v) ->
        let ctx = EntityContext
                { field_entities       = eix
                , field_selfId         = k
                , field_frameCount     = fct
                , field_frameTimestamp = cft
                , field_resources      = res }
        in (k,) <$> liftIO (runQ (entityUpdate v ctx))

    -- List of directed actions resulting from update + global ones
    let updateActions = concatMap (snd.snd) updateResult
    let directedActions = updateActions <> globalActions
    let (actionsAtEntity, actionsAtWorld) = partitionActions directedActions
    debugPrintActions actionsAtEntity

    -- Preform world actions
    newEntitiesActionsList <- concat <$> mapM handleAndInsert actionsAtWorld

    let allActionsAtEntity = newEntitiesActionsList <> actionsAtEntity
    handleEntityActions allActionsAtEntity

    -- Map of entity action grouped by target entity id
    let directedActionsMap :: HashMap EntityId [EntityAction]
        directedActionsMap = HashMap.fromListWith (<>) $ map f allActionsAtEntity
            where f d = (d^.entityId, [d^.action])

    -- Set of all entity ids that changed in this frame
    let changedList = hashNub $
            (map fst updateResult) <> (map (view entityId) allActionsAtEntity)
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

    -- Reindex changed entities
    toUpdateOnIndexList <- forM preChangeList $ \ewid -> do
        let i = ewid^.entityId
        newEwid <- liftIO $ lookupById i eix
        return (i, Just $ ewid^.entity, view entity <$> newEwid)
    let toReindexList = toUpdateOnIndexList
    mapM_ (reindex eix) toReindexList

    -- Update list of activated entities to be processed in next frame
    directedList <- liftIO $ lookupManyById (HashMap.keys directedActionsMap) eix
    let f = filter (\e -> entityKind (e^.entity) /= EntityKind_Dynamic)
    writeIORef (eix^.activatedSet) $
        HashSet.fromList $ map (view entityId) $ f directedList

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
            Nothing -> return []
            Just (en, opts) -> do
                i <- insert en eix
                when (opts^.tagAsCamera) $ addTag EntityIndexTag_Camera i eix
                return $ map (DirectedEntityAction i) (opts^.actions)

    action = ff#action

    ----------------------------------------------------------------------------
    debugPrintActions
        | fct > 3   = mapM_ prt . filter (noSetMoveVector . view action)
        | otherwise = const (return ())
    prt x = putStrLn $ printf "%d: %s" fct (show @String x)

reindex :: MonadIO m
    => EntityIndex
    -> (EntityId, Maybe Entity, Maybe Entity)
    -> m ()
reindex eix (i, mo, mn) = case (mo, mn) of
    (Nothing, Nothing) -> return ()
    (Nothing, Just nv) -> addOnIndex nv
    (Just ov, Nothing) -> deleteFromIndex ov >> printDebug "Delete Entity: " ov
    (Just ov, Just nv) -> updateInIndex ov nv
    where
    addOnIndex nv = do
        let ki = FullMap.lookup (entityKind nv) (eix^.spatialIndex)
        let pos = getPosM nv
        -- when (entityKind nv == EntityKind_Dynamic) $ printDebug "Add Entity: " nv
        whenJust pos $ \p -> SpatialIndex.insert p i ki

    printDebug msg ov = putStrLn
        $ msg
        <> show i <> ", "
        <> (show $ entityKind ov) <> ", "
        <> "Name: " <> (fromMaybe "" $ fmap toString $ ov^.oracleName)

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

    getPosM x = x^?oracleLocation.traverse._Wrapped

insert :: MonadIO m => Entity -> EntityIndex -> m EntityId
insert e eix = do
    old <- readIORef (eix^.lastId)
    let !newUnique = maybe 0 (\x -> x^.unique+1) old
    let mkId = EntityId newUnique
    o <- VectorIndex.insertWithKey (flip EntityWithId e . mkId) (eix^.entities)
    let newId = mkId o
    reindex eix (newId, Nothing, Just e)
    when (entityKind e == EntityKind_Dynamic) $
        modifyIORef (eix^.dynamicIndex) $ HashSet.insert newId
    writeIORef (eix^.lastId) (Just newId)
    return newId

insertStaticShape :: MonadIO m => StaticShape -> EntityIndex -> m ()
insertStaticShape ss eix =
    SpatialIndex.insert (ss^.center) ss (eix^.ff#staticShapes)

addTag :: MonadIO m => EntityIndexTag -> EntityId -> EntityIndex -> m ()
addTag t eid eix = modifyIORef (eix^.ff#tags) $ Map.insert t eid

lookupById :: MonadQ m => EntityId -> EntityIndex -> m (Maybe EntityWithId)
lookupById eid = liftQ . lookupVector eid . view entities

lookupByTag :: MonadQ m => EntityIndexTag -> EntityIndex -> m (Maybe EntityWithId)
lookupByTag t eix = liftQ $ do
    mi <- Map.lookup t <$> Q (readIORef $ eix^.ff#tags)
    maybe (return Nothing) (flip lookupById eix) mi

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
        EntityKind_Passive -> 4
        EntityKind_Dynamic -> 2

    -- expandRange :: Num x => x -> BBox x -> BBox x
    expandRange e bb = bb
        & minPoint %~ fmap (\x -> x-e)
        & maxPoint %~ fmap (\x -> x+e)

lookupInRadius :: MonadQ m
    => EntityKind -> Location -> Distance -> EntityIndex -> m [EntityWithId]
lookupInRadius k loc d eix =
    filter isInRadius <$> lookupInRange k queryRange eix
    where
    queryRange = makeRangeSquare loc d
    isInRadius x = maybe False (isWithinDistance d loc) (x^.entity.oracleLocation)

lookupStaticShapesInRange :: MonadQ m => RangeBBox -> EntityIndex -> m [StaticShape]
lookupStaticShapesInRange r eix =
    liftQ $ Q $ SpatialIndex.lookup r (eix^.ff#staticShapes)

makeRangeSquare :: Location -> Distance -> RangeBBox
makeRangeSquare loc d = mkBBoxCenter (loc^._Wrapped) (pure . (*2) $ d^._Wrapped)

lookupManyById :: (Foldable t, MonadQ m)
    => t EntityId -> EntityIndex -> m [EntityWithId]
lookupManyById s e = catMaybes <$> mapM (flip lookupById e) (toList s)

