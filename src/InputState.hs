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

    , isPartialMatch
    ) where

import Delude
import qualified Data.Vector as Vector
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Map as PrefixMap
import qualified Data.Zipper as Zipper

import Engine (userState, EngineState, Key)
import Types (Game, St)
import Types.EntityAction
import Types.Entity.Common (EntityId)
import Types.Entity.PassiveType (InteractionName)
import Types.InputState
import Types.Equipment
import InputState.Actions
import GameState
import Focus

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
    common <- use (userState.inputState.inputKeymap.ff#keymapCommon)
    modeKeymap <- use (userState.inputState.inputKeymap.ff#keymap)
    return $ case Map.lookup currentInputMode modeKeymap of
        Nothing -> common
        Just km -> PrefixMap.union km common -- left biased union

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

handleSelectKind :: SelectState -> Seq Char -> Game ()
handleSelectKind s selseq = do
    case s^.selectKind of
        SelectKind_Pickup v -> selectLookup v selectPickup
        SelectKind_Drop   v -> selectLookup v selectDrop
        SelectKind_Focus  v -> selectLookup v selectFocus
        SelectKind_MoveTo v -> selectLookup v selectMoveTarget
        SelectKind_Action v -> selectLookup v selectAction
    unless (isPartialMatch smap selseq) endSelect
    where
    smap = s^.selectMap
    selectLookup :: SelectValues a -> (a -> Game ()) -> Game ()
    selectLookup v f
        | Vector.length vs == 1 =
            whenJust (Vector.indexM vs 0) $ \a -> f a >> endSelect
        | otherwise = case PrefixMap.lookup (toList selseq) smap of
            Nothing -> return ()
            Just i -> case Vector.indexM vs i of
                Nothing -> return ()
                Just a  -> f a
        where
        vs = v^.values

selectPickup :: EntityId -> Game ()
selectPickup eid = withFocusEntityWithId $ \fe -> do
    mapM_ pickupItem =<< filterFitItems fe =<< lookupEntities [eid]
    selectFocus eid

selectDrop :: EntityId -> Game ()
selectDrop eid = dropItem eid >> selectFocus eid

selectFocus :: EntityId -> Game ()
selectFocus = zoomInputState . assign (inventoryState.focusedItem) . Just

selectMoveTarget :: ItemMoveTarget -> Game ()
selectMoveTarget m = whenJustM getFocusedItem $ \e -> case m of
    ItemMoveTarget_Ground          -> dropItem e
    ItemMoveTarget_Container       -> passToContainer e
    ItemMoveTarget_Backpack        -> passToBackpack e
    ItemMoveTarget_EquipmentSlot s -> passToSlot e s
    where
    passToContainer e = whenJustM getInventoryContainer
        $ passItemTo e . view entityId
    passToBackpack e = whenJustM (focusEquipmentSlot EquipmentSlot_Backpack)
        $ passItemTo e . view entityId

    passToSlot e s = withFocusId $ \fi -> passItemToSlot e fi s

selectAction :: (EntityId, InteractionName) -> Game ()
selectAction (eid, eua) = whenJustM (focusEntityId)
    $ actOnEntity eid . EntityAction_Interact (Just eua)

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
    StatusMode StatusMenu_StoryDialog -> storyNextPage
    _                                 -> return ()
    where
    storyNextPage = whenJustM (use storyDialog) $ \sd -> do
        actOnEntity (sd^.entityId) $ EntityAction_Dialog DialogAction_NextPage
        if Zipper.isRightmost $ sd^.ff#dialogPages
        then storyDialog .= Nothing >> inputActionEscape
        else storyDialog.traverse.ff#dialogPages %= Zipper.right

    storyDialog :: Lens' (EngineState St) (Maybe StoryDialogState)
    storyDialog = userState.inputState.ff#storyDialog

