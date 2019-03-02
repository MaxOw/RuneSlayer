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

--------------------------------------------------------------------------------

-- Get focused entity (if any)
focusEntity :: Game (Maybe Entity)
focusEntity = do
    mfi <- use $ userState.gameState.focusId
    case mfi of
        Nothing -> return Nothing
        Just fi -> fmap (view entity) <$> lookupEntity fi

-- Get location of a focused entity (if any)
focusLocation :: Game (Maybe Location)
focusLocation = (view (oracle.location) =<<) <$> focusEntity

focusEntityId :: Game (Maybe EntityId)
focusEntityId = use (userState.gameState.focusId)

withFocusId :: (EntityId -> Game ()) -> Game ()
withFocusId = whenJustM focusEntityId

-- Get items within pickup range of a focused entity
focusItemsInRange :: Game [EntityWithId]
focusItemsInRange = focusLocation >>= \case
    Nothing -> return []
    Just lc -> do
        eix <- use $ userState.gameState.entities
        es <- lookupInRange EntityKind_Item (queryRange lc) eix
        return $ filter (isItemInRange lc . view (entity.oracle)) es
    where
    queryRange loc = mkBBoxCenter (loc^._Wrapped)
        (pure . (*2) $ defaultPickupRange^._Wrapped)

    isItemInRange loc x = withinRange loc (x^.location) && isJust (x^.itemKind)
    withinRange l xl = nothingFalse xl $ isWithinDistance defaultPickupRange l

focusItemsInInventory :: Game [EntityWithId]
focusItemsInInventory = do
    mbp <- focusEquipmentSlot EquipmentSlot_Backpack
    mf <- focusEntity
    let es = mf^.traverse.oracle.equipment.traverse.to contentList
    let bs = mbp^..traverse.entity.oracle.content.traverse.traverse
    lookupEntities (es <> bs)

focusEquipmentSlot :: EquipmentSlot -> Game (Maybe EntityWithId)
focusEquipmentSlot es = do
    mf <- focusEntity
    let meq = mf^?traverse.oracle.equipment.traverse
    case Equipment.lookupSlot es =<< meq of
        Nothing -> return Nothing
        Just  i -> lookupEntity i

--------------------------------------------------------------------------------

lookupEntities :: Foldable t => t EntityId -> Game [EntityWithId]
lookupEntities is = do
    es <- use $ userState.gameState.entities
    EntityIndex.lookupManyById is es

lookupEntity :: EntityId -> Game (Maybe EntityWithId)
lookupEntity i = do
    es <- use $ userState.gameState.entities
    EntityIndex.lookupById i es

