module EntityIndex
    ( EntityIndex

    , emptyIndex
    , updateIndex
    , addToIndex
    , getLastId
    , entitiesInRange
    , lookupEntityById
    , queryIndex

    , nothingFalse
    , nothingFalse2
    ) where

import Delude
import Text.Printf
import Types.Entity
import Types.Entity.Common (EntityId (..))
import qualified Data.HashMap.Strict as HashMap

--------------------------------------------------------------------------------

emptyIndex :: EntityIndex
emptyIndex = EntityIndex
   { entityIndexLastId   = EntityId 0
   , entityIndexEntities = HashMap.empty
-- , entityIndexLocation = undefined
   }

noSetMoveVector :: EntityAction -> Bool
noSetMoveVector (EntityAction_SetMoveVector {}) = False
noSetMoveVector _                               = True

updateIndex
    :: MonadIO m
    => [DirectedEntityAction] -> Word32 -> EntityIndex -> m EntityIndex
updateIndex acs fct eix = do
    mapM_ prt $ filter (noSetMoveVector . view action) directedActions
    return $ eix
        { entityIndexEntities = actedOnEntities
    -- , entityIndexLocation = rebuildLocationIndex actedOnEntities
        }
    where
    prt x = putStrLn ((printf "%d: %s" fct (show x :: String)) :: String)
    h = HashMap.mapWithKey updateEntity $ entityIndexEntities eix
    directedActions = concatMap snd (HashMap.elems h) <> acs
    updatedEntities = HashMap.mapMaybe fst h
    actedOnEntities = foldr performAction updatedEntities directedActions

    updateEntity k v = entityUpdate v ctx
        where
        ctx = EntityContext
            { entityContext_entities   = eix
            , entityContext_selfId     = k
            , entityContext_frameCount = fct
            }

    performAction (DirectedEntityAction eid act)
        = HashMap.adjust (flip entityActOn act) eid

getLastId :: EntityIndex -> Maybe EntityId
getLastId eix
    | eid <= EntityId 0 = Nothing
    | otherwise         = Just eid
    where eid = entityIndexLastId eix

addToIndex :: Entity -> EntityIndex -> EntityIndex
addToIndex e eix = eix
    { entityIndexLastId   = newId
    , entityIndexEntities = newMap
 -- , entityIndexLocation = addLocationIndex (queryLocation e) newId
    }
    where
    newId  = EntityId $ unEntityId (entityIndexLastId eix) + 1
    newMap = HashMap.insert newId e $ entityIndexEntities eix

lookupEntityById :: EntityId -> EntityIndex -> Maybe Entity
lookupEntityById eid eix = HashMap.lookup eid $ entityIndexEntities eix

-- TODO: For not this just returs list of all entities but in the future we
-- should be quering for only visible entities within ViewRange from a space
-- partitioning tree of entities.
type ViewRange = ()
entitiesInRange :: ViewRange -> EntityIndex -> [Entity]
entitiesInRange _ = HashMap.elems . entityIndexEntities

-- TODO: this again is a naive filtering of all entities without emploing any
-- sort of index what so ever. Obviously this needs to be improved in the future
-- once it will become a bottleneck.
queryIndex :: (EntityOracle -> Bool) -> EntityIndex -> [EntityWithId]
queryIndex f = filter (f . view (entity.oracle)) . allEntities

allEntities :: EntityIndex -> [EntityWithId]
allEntities = map (uncurry EntityWithId) . HashMap.toList . entityIndexEntities

--------------------------------------------------------------------------------

nothingFalse :: Maybe a -> (a -> Bool) -> Bool
nothingFalse Nothing  _ = False
nothingFalse (Just a) f = f a

nothingFalse2 :: Maybe a -> Maybe b -> (a -> b -> Bool) -> Bool
nothingFalse2 Nothing        _  _ = False
nothingFalse2       _  Nothing  _ = False
nothingFalse2 (Just a) (Just b) f = f a b

