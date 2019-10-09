module Focus where

import Delude
import qualified Data.Set as Set
import Entity
import Types
import Types.GameState (gameState)
import Types.Entity.Common
import Types.Entity.Passive
import Types.Entity.Agent (AgentKind (..), agentKind)
import Equipment (EquipmentSlot(..), contentList)
import qualified Equipment
import EntityIndex
import GameState.Query
import InputState.Actions

--------------------------------------------------------------------------------

-- Get focused entity (if any)
focusEntity :: Game (Maybe Entity)
focusEntity = do
    mfi <- use $ gameState.focusId
    case mfi of
        Nothing -> return Nothing
        Just fi -> fmap (view entity) <$> lookupEntity fi

-- Get location of a focused entity (if any)
focusLocation :: Game (Maybe Location)
focusLocation = (view (oracleLocation) =<<) <$> focusEntity

-- Get location of a camera
cameraLocation :: Game (Maybe Location)
cameraLocation = do
    eix <- use $ gameState.entities
    mei <- lookupByTag EntityIndexTag_Camera eix
    return (view (entity.oracleLocation) =<< mei)

focusEntityId :: Game (Maybe EntityId)
focusEntityId = use (gameState.focusId)

withFocusId :: (EntityId -> Game ()) -> Game ()
withFocusId = whenJustM focusEntityId

withFocusEntityWithId :: (EntityWithId -> Game ()) -> Game ()
withFocusEntityWithId = whenJustM $ runMaybeT $
    MaybeT . lookupEntity =<< MaybeT (use $ gameState.focusId)

focusEntityKindInRange :: EntityKind -> Distance -> Game [EntityWithId]
focusEntityKindInRange k d = focusLocation >>= \case
    Nothing -> return []
    Just lc -> lookupInRadius k lc d =<< use (gameState.entities)

-- Get items within pickup range of a focused entity
focusItemsInRange :: Game [EntityWithId]
focusItemsInRange = filter isItem
    <$> focusEntityKindInRange EntityKind_Passive defaultPickupRange
    where
    isItem x = Set.member PassiveKind_Item
        $ x^.entity.oraclePassiveType.traverse.passiveKind

focusInteractionsInRange :: Game [(EntityWithId, InteractionName)]
focusInteractionsInRange = concatMap f <$> allInRange
    where
    allInRange = (++)
        <$> focusEntityKindInRange EntityKind_Passive defaultActionRange
        <*> focusEntityKindInRange EntityKind_Dynamic defaultActionRange
    f x = map (x,) $ x^.entity.oracleInteractions.traverse

focusNearestInteractionInRange :: Game (Maybe (EntityWithId, InteractionName))
focusNearestInteractionInRange = focusLocation >>= \case
    Nothing -> return Nothing
    Just lc -> do
        ns <- focusInteractionsInRange
        let calcDist = fmap (calcDistance lc) . view (_1.entity.oracleLocation)
        let eds = mapMaybe (\n -> (n,) <$> calcDist n) ns
        return $ viaNonEmpty head $ map fst $ sortWith snd eds

focusNPCsInRange :: Game [EntityWithId]
focusNPCsInRange = filter isNPC
    <$> focusEntityKindInRange EntityKind_Dynamic defaultActionRange
    where
    isNPC x = x^?entity.oracleAgentType.traverse.agentKind == Just AgentKind_NPC

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

