module Focus where

import Delude
import Engine (userState)
import Engine.Common.Types (mkBBoxCenter)
import Entity
import Types
import Types.Entity.Common
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
focusLocation = (view (oracleLocation) =<<) <$> focusEntity

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
        return $ filter (isItemInRange lc . view entity) es
    where
    queryRange loc = mkBBoxCenter (loc^._Wrapped)
        (pure . (*2) $ defaultPickupRange^._Wrapped)

    isItemInRange loc x
        = withinRange loc (x^.oracleLocation) && isJust (x^.oracleItemType)
    withinRange l xl = nothingFalse xl $ isWithinDistance defaultPickupRange l

focusItemsInInventory :: Game [EntityWithId]
focusItemsInInventory = do
    mbp <- focusEquipmentSlot EquipmentSlot_Backpack
    mf <- focusEntity
    let es = mf^.traverse.oracleEquipment.traverse.to contentList
    let bs = mbp^..traverse.entity.oracleContent.traverse.traverse
    lookupEntities (es <> bs)

focusEquipmentSlot :: EquipmentSlot -> Game (Maybe EntityWithId)
focusEquipmentSlot es = do
    mf <- focusEntity
    let meq = mf^?traverse.oracleEquipment.traverse
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

