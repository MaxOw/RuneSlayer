module InputState
    ( module Types.InputState

    , getMode, setMode
    , isActionActive, activateAction, deactivateAction
    -- , getInputKeymap
    , registerDeactivation, popDeactivators
    , getSelectState
    , matchKeymap
    , appendHist
    , getInputString, appendInputString, backspaceInputString, clearInputString
    , startSelect, appendSelect, backspaceSelect, endSelect
    , getInventoryContainer
    , clearInventoryState
    , unfocusItem
    , getFocusedItem
    , handleSelectKind
    , inputActionEscape
    , toggleViewPanel, isPanelVisible
    , nextPage
    , showActionKeySeqs
    , startSelectMoveTo

    , isPartialMatch
    ) where

import Delude
import qualified Data.Vector as Vector
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Map as PrefixMap

import Engine (userState, Key)
import Entity
import Types (Game)
import Types.Entity.Common (EntityId)
import Types.Entity.PassiveType (InteractionName)
import Types.InputState
import Types.Equipment
import InputState.Actions
import GameState
import GameState.Query (canFitIntoContainer)
import Focus
import qualified Story
import qualified Equipment

--------------------------------------------------------------------------------

isActionActive :: ActiveAction -> Game Bool
isActionActive action = do
    activeMap <- use (userState.inputState.active)
    let val = fromMaybe 0 $ Map.lookup action activeMap
    return (val > 0)

activateAction :: ActiveAction -> Game ()
activateAction action = do
    modifying (userState.inputState.active) $ \activeMap ->
        let val = fromMaybe 0 $ Map.lookup action activeMap
        in Map.insert action (val + 1) activeMap

deactivateAction :: ActiveAction -> Game ()
deactivateAction action = do
    modifying (userState.inputState.active) $ \activeMap ->
        let val = fromMaybe 0 $ Map.lookup action activeMap
        in Map.insert action (max 0 $ val - 1) activeMap

registerDeactivation :: Key -> InputAction -> Game ()
registerDeactivation kp action = zoomInputState $ do
    deactivators %= Map.insertWith (<>) kp [action]

popDeactivators :: Key -> Game [InputAction]
popDeactivators kp = zoomInputState $ do
    ia <- concat . Map.lookup kp <$> use deactivators
    deactivators %= Map.delete kp
    return ia

--------------------------------------------------------------------------------

getSelectState :: Game (Maybe SelectState)
getSelectState = use (userState.inputState.selectState)

getModeKeymap :: Game Keymap
getModeKeymap = do
    currentInputMode <- getMode
    modeKeymap <- use (userState.inputState.inputKeymap.ff#keymap)
    return $ fromMaybe PrefixMap.empty $ Map.lookup currentInputMode modeKeymap

-- Mach keymap for current input mode and keypress hist
-- Clear hist on success or total failure but leave it be on partial match
matchKeymap :: Seq Keypress -> Game (Maybe InputAction)
matchKeymap keyseq = do
    km <- getModeKeymap
    unless (isPartialMatch km keyseq) clearHist
    return $ PrefixMap.lookup (toList keyseq) km

isPartialMatch :: Ord k => PrefixMap k v -> Seq k -> Bool
isPartialMatch m s = not (isValid || PrefixMap.member slist m)
    where
    isValid = null $ take 2 $ filter (isPrefixOf slist) $ PrefixMap.keys m
    slist = toList s

appendHist :: Keypress -> Game (Seq Keypress)
appendHist kp = userState.inputState.hist <%= (|> kp)

clearHist :: Game ()
clearHist = userState.inputState.hist .= empty

--------------------------------------------------------------------------------

startSelect :: Ord a => (SelectValues a -> SelectKind) -> [a] -> Game ()
startSelect _ [] = return ()
startSelect f vs = do
    zoomInputState $ selectState .= Just ss
    handleSelectKind ss Empty
    where
    (k, m) = makeSelectMap vs
    ss = SelectState
        { field_selectKind    = f k
        , field_selectMap     = m
        , field_currentPrefix = empty
        }

    makeSelectMap es = (SelectValues (Vector.fromList es) revMap, selMap)
        where
        revMap = Map.fromList $ zip es ps
        selMap = PrefixMap.fromList $ zip ps $ map fst $ zip [0..] es
        ps = allSelectors prefixDepth
        prefixDepth = ceiling @Float
            $ logBase (genericLength defaultSelectors) (genericLength es)

        allSelectors :: Int -> [String]
        allSelectors x = if x <= 0 then [] else go x
            where
            go i = if i <= 0 then [[]]
                else (:) <$> defaultSelectors <*> go (i-1)

type GameSeq = Game (Maybe (Game ()))

handleSelectKind :: SelectState -> Seq Char -> Game ()
handleSelectKind s selseq = do
    mSeqAction <- case s^.selectKind of
        SelectKind_Pickup v -> selectLookup v selectPickup
        SelectKind_Drop   v -> selectLookup v selectDrop
        SelectKind_Focus  v -> selectLookup v selectFocus
        SelectKind_Move   v -> selectLookup v selectMove
        SelectKind_MoveTo v -> selectLookup v selectMoveTarget
        SelectKind_Action v -> selectLookup v selectAction
    unless (isPartialMatch smap selseq) endSelect
    whenJust mSeqAction id
    where
    smap = s^.selectMap
    selectLookup
        :: SelectValues a -> (a -> GameSeq) -> GameSeq
    selectLookup v f
        | Vector.length vs == 1 = case Vector.indexM vs 0 of
            Nothing -> return Nothing
            Just  a -> f a >>= \mr -> endSelect >> return mr
        | otherwise = case PrefixMap.lookup (toList selseq) smap of
            Nothing -> return Nothing
            Just i -> case Vector.indexM vs i of
                Nothing -> return Nothing
                Just a  -> f a
        where
        vs = v^.values

selectPickup :: EntityId -> GameSeq
selectPickup eid = do
    withFocus $ \fe -> do
        mapM_ pickupItem =<< filterFitItems fe =<< lookupEntities [eid]
        void $ selectFocus eid
    return Nothing

selectDrop :: EntityId -> GameSeq
selectDrop eid = dropItem eid >> selectFocus eid >> return Nothing

selectMove :: EntityId -> GameSeq
selectMove eid = selectFocus eid >> return (Just startSelectMoveTo)

selectFocus :: EntityId -> GameSeq
selectFocus eid = do
    zoomInputState . assign (inventoryState.focusedItem) $ Just eid
    return Nothing

selectMoveTarget :: ItemMoveTarget -> GameSeq
selectMoveTarget m = do
    whenJustM getFocusedItem $ \e -> case m of
        ItemMoveTarget_Ground          -> dropItem e
        ItemMoveTarget_Container       -> passToContainer e
        ItemMoveTarget_Backpack        -> passToBackpack e
        ItemMoveTarget_EquipmentSlot s -> passToSlot e s
    return Nothing
    where
    passToContainer e = whenJustM getInventoryContainer
        $ passItemTo e . view entityId
    passToBackpack e = whenJustM (focusEquipmentSlot EquipmentSlot_Backpack)
        $ passItemTo e . view entityId

    passToSlot e s = withFocusId $ \fi -> passItemToSlot e fi s

selectAction :: (EntityId, InteractionName) -> GameSeq
selectAction (eid, eua) = do
    whenJustM (focusEntityId) $ actOnEntity eid . EntityAction_Interact (Just eua)
    return Nothing

unfocusItem :: Game ()
unfocusItem = zoomInputState $ inventoryState.focusedItem .= Nothing

getFocusedItem :: Game (Maybe EntityId)
getFocusedItem = zoomInputState $ use (inventoryState.focusedItem)

appendSelect :: Char -> Game (Seq Char)
appendSelect ch = zoomInputState $ selectState._Just.currentPrefix <%= (|> ch)

backspaceSelect :: Game ()
backspaceSelect = zoomInputState $ selectState._Just.currentPrefix %= dropR1

endSelect :: Game ()
endSelect = zoomInputState $ selectState .= Nothing

--------------------------------------------------------------------------------

toggleViewPanel :: PanelName -> Game ()
toggleViewPanel x = zoomInputState $ visiblePanels %= toggleSet x

isPanelVisible :: PanelName -> Game Bool
isPanelVisible pname = zoomInputState $ uses visiblePanels (Set.member pname)

--------------------------------------------------------------------------------

nextPage :: Game ()
nextPage = getMode >>= \case
    StoryDialogMode -> Story.nextPage
    _               -> return ()

--------------------------------------------------------------------------------

startSelectMoveTo :: Game ()
startSelectMoveTo = getFocusedItem >>= \case
    Nothing -> return () -- Messages.addInfo "No item selected!"
    Just fi -> lookupEntity fi >>= \case
        Nothing -> return ()
        Just fe -> startSelect SelectKind_MoveTo =<< getValidTargets fe
    where
    getValidTargets fe = do
        let fs = toList $ fe^.entity.oracleFittingSlots.traverse
        mb <- getBackpackTarget fe
        mc <- getContainerTarget fe
        let mg = Just ItemMoveTarget_Ground
        mct <- getCurrentTarget fe
        let is = map (Just . ItemMoveTarget_EquipmentSlot) fs <> [mb, mc, mg]
        return $ catMaybes $ filter (/= mct) is

    getBackpackTarget = fitForTarget
        (focusEquipmentSlot EquipmentSlot_Backpack)
        ItemMoveTarget_Backpack

    getContainerTarget = fitForTarget
        getInventoryContainer
        ItemMoveTarget_Container

    fitForTarget getT mt fe = getT >>= \case
        Nothing -> return Nothing
        Just bi -> bool Nothing (Just mt) <$> canFitIntoContainer fe bi

    -- what a mess...
    getCurrentTarget fe = do
        if isJust $ fe^.entity.oracleLocation
        then return $ Just ItemMoveTarget_Ground
        else do
            mbp <- focusEquipmentSlot EquipmentSlot_Backpack
            let bp = mbp^..traverse.entity.oracleContent.traverse.traverse
            if any (fe^.entityId ==) bp
            then return $ Just ItemMoveTarget_Backpack
            else do
                ct <- map (view entityId) <$> focusItemsInContainer
                if any (fe^.entityId ==) ct
                then return $ Just ItemMoveTarget_Container
                else getCurrentEquipmentTarget fe

    getCurrentEquipmentTarget fe = do
        mf <- focusEntity
        case mf^?traverse.oracleEquipment.traverse of
            Nothing -> return Nothing
            Just eq -> do
                mapM_ print $ Equipment.slotsList eq
                let ms = find ((fe^.entityId ==) . snd) $ Equipment.slotsList eq
                return $ ItemMoveTarget_EquipmentSlot . fst <$> ms

