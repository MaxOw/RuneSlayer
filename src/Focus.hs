module Focus where

import Delude
import qualified Data.Set as Set
import Engine (userState)
import Entity
import Types
import Types.EntityAction (UseActionName)
import Types.Entity.Common
import Types.Entity.Passive
import Equipment (EquipmentSlot(..), contentList)
import qualified Equipment
import EntityIndex
import GameState.Query
import InputState.Actions

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

-- Get location of a camera
cameraLocation :: Game (Maybe Location)
cameraLocation = do
    eix <- use $ userState.gameState.entities
    mei <- lookupByTag EntityIndexTag_Camera eix
    return (view (entity.oracleLocation) =<< mei)

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
        es <- lookupInRadius EntityKind_Passive lc defaultPickupRange eix
        return $ filter isItem es
    where
    isItem x = Set.member PassiveKind_Item
        $ x^.entity.oraclePassiveType.traverse.passiveKind

focusActionsInRange :: Game [(EntityWithId, UseActionName)]
focusActionsInRange = focusLocation >>= \case
    Nothing -> return []
    Just lc -> do
        eix <- use $ userState.gameState.entities
        es <- lookupInRadius EntityKind_Passive lc defaultActionRange eix
        return $ concatMap f es
    where
    f x = map (x,) $ x^.entity.oracleUseActions.traverse

focusItemsInInventory :: Game [EntityWithId]
focusItemsInInventory = do
    mbp <- focusEquipmentSlot EquipmentSlot_Backpack
    mf <- focusEntity
    let es = mf^.traverse.oracleEquipment.traverse.to contentList
    let bs = mbp^..traverse.entity.oracleContent.traverse.traverse
    lookupEntities (es <> bs)

focusItemsInContainer :: Game [EntityWithId]
focusItemsInContainer = getInventoryContainer >>= \mc ->
    lookupEntities $ mc^.traverse.entity.oracleContent.traverse

focusEquipmentSlot :: EquipmentSlot -> Game (Maybe EntityWithId)
focusEquipmentSlot es = do
    mf <- focusEntity
    let meq = mf^?traverse.oracleEquipment.traverse
    case Equipment.lookupSlot es =<< meq of
        Nothing -> return Nothing
        Just  i -> lookupEntity i

