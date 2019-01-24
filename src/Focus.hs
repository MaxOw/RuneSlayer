module Focus where

import Delude
import Engine (userState)
import Types
import Types.Entity
import Types.Entity.Common
import Types.GameState
import EntityIndex

liftGame :: (St -> a) -> Game a
liftGame f = f <$> use userState

-- Get location of a focused entity (if any)
focusLocation :: St -> Maybe Location
focusLocation st = view location =<< entityOracle <$> focusEntity st

focusEntityId :: St -> Maybe EntityId
focusEntityId = view (gameState.focusId)

withFocusId :: (EntityId -> Game ()) -> Game ()
withFocusId = whenJustM (liftGame focusEntityId)

-- Get focused entity (if any)
focusEntity :: St -> Maybe Entity
focusEntity st = me
    where
    mid = st^.gameState.focusId
    eix = st^.gameState.entities
    me = flip lookupEntityById eix =<< mid

-- Get items within pickup range of a focused entity
focusItemsInRange :: St -> [EntityWithId]
focusItemsInRange st = queryIndex rangeQuery $ st^.gameState.entities
    where
    rangeQuery x = withinRange (x^.location) && isJust (x^.itemKind)

    withinRange loc
        = nothingFalse2 (focusLocation st) loc
        $ isWithinDistance defaultPickupRange
