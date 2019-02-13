module Focus where

import Delude
import Engine (userState)
import Engine.Common.Types (mkBBoxCenter)
import Types
import Types.Entity
import Types.Entity.Common
import Types.GameState
import Equipment (EquipmentSlot(..), contentList)
import qualified Equipment
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
focusEntity st = view entity <$> me
    where
    mid = st^.gameState.focusId
    eix = st^.gameState.entities
    me = flip lookupEntityById eix =<< mid

-- Get items within pickup range of a focused entity
focusItemsInRange :: St -> [EntityWithId]
focusItemsInRange st = case focusLocation st of
    Nothing -> []
    Just loc -> queryItemsInPickupRange loc
    where
    queryItemsInPickupRange loc
        = filter (isItemInRange . view (entity.oracle))
        $ dynamicEntitiesInRange (queryRange loc) $ st^.gameState.entities

    isItemInRange x = withinRange (x^.location) && isJust (x^.itemKind)
    queryRange loc = mkBBoxCenter (fmap realToFrac $ loc^._Wrapped)
        (fmap realToFrac $ pure $ defaultPickupRange^._Wrapped)

    withinRange loc
        = nothingFalse2 (focusLocation st) loc
        $ isWithinDistance defaultPickupRange

focusItemsInInventory :: St -> [EntityWithId]
focusItemsInInventory st = lookupEntities st (es <> bs)
    where
    mbp = focusEquipmentSlot st EquipmentSlot_Backpack
    bs = mbp^..traverse.entity.oracle.content.traverse.traverse
    es = st^.to focusEntity.traverse.oracle.equipment.traverse.to contentList

lookupEntities :: St -> [EntityId] -> [EntityWithId]
lookupEntities st = mapMaybe $ \i -> lookupEntityById i (st^.gameState.entities)

lookupEntity :: St -> EntityId -> Maybe EntityWithId
lookupEntity st i = lookupEntityById i (st^.gameState.entities)

focusEquipmentSlot :: St -> EquipmentSlot -> Maybe EntityWithId
focusEquipmentSlot st es = lookupEntity st
      =<< Equipment.lookupSlot es
      =<< st^?to focusEntity.traverse.oracle.equipment.traverse

