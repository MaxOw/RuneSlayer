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
    , unfocusItem, getFocusedItem
    , handleSelectKind
    , inputActionEscape
    , toggleViewPanel, isPanelVisible

    , isPartialMatch
    ) where

import Delude
import qualified Data.Vector as Vector
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Map as PrefixMap

import Engine (userState)
import Types (Game)
import Types.Entity.Common (EntityId)
import Types.InputState
import GameState

--------------------------------------------------------------------------------

zoomInputState :: InputStateM a -> Game a
zoomInputState = zoom (userState.inputState)

--------------------------------------------------------------------------------

getMode :: Game InputMode
getMode = use (userState.inputState.mode)

setMode :: InputMode -> Game ()
setMode m = assign (userState.inputState.mode) m

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

registerDeactivation :: Keypress -> InputAction -> Game ()
registerDeactivation kp action = zoomInputState $ do
    deactivators %= Map.insertWith (<>) kp [action]

popDeactivators :: Keypress -> Game [InputAction]
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
    common <- use (userState.inputState.commonKeymap)
    modeKeymap <- use (userState.inputState.inputKeymap)
    return $ case (Map.lookup currentInputMode modeKeymap) of
        Nothing -> common
        Just km -> PrefixMap.union common km -- left biased union

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

getInputString :: Game Text
getInputString = uses (userState.inputState.ff#inputString)
    (fromList @Text . toList)

appendInputString :: Char -> Game ()
appendInputString x = userState.inputState.ff#inputString %= (|> x)

backspaceInputString :: Game ()
backspaceInputString = userState.inputState.ff#inputString %= dropR1

clearInputString :: Game ()
clearInputString = userState.inputState.ff#inputString .= mempty

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
        SelectPickup v -> selectLookup selectPickup v
        SelectDrop   v -> selectLookup selectDrop   v
        SelectFocus  v -> selectLookup selectFocus  v
    unless (isPartialMatch smap selseq) endSelect
    where
    smap = s^.selectMap
    selectLookup f v
        | Vector.length vs == 1 = whenJust (Vector.indexM vs 0) $ \a -> do
            f a >> endSelect
        | otherwise = case PrefixMap.lookup (toList selseq) smap of
            Nothing -> return ()
            Just i -> case Vector.indexM vs i of
                Nothing -> return ()
                Just a  -> f a
        where
        vs = v^.values

selectPickup :: EntityId -> Game ()
selectPickup eid = pickupItem eid >> selectFocus eid

selectDrop :: EntityId -> Game ()
selectDrop eid = dropItem eid >> selectFocus eid

selectFocus :: EntityId -> Game ()
selectFocus = zoomInputState . assign (inventoryState.focusedItem) . Just

unfocusItem :: Game ()
unfocusItem = zoomInputState $ inventoryState.focusedItem .= Nothing

getFocusedItem :: Game (Maybe EntityId)
getFocusedItem = zoomInputState $ use (inventoryState.focusedItem)

appendSelect :: Char -> Game (Seq Char)
appendSelect ch = zoomInputState $ selectState._Just.currentPrefix <%= (|> ch)

backspaceSelect :: Game ()
backspaceSelect = zoomInputState $ selectState._Just.currentPrefix %= dropR1

dropR1 :: Snoc (t x) (t x) x x => t x -> t x
dropR1 (l:>_) = l
dropR1 x = x

endSelect :: Game ()
endSelect = zoomInputState $ selectState .= Nothing

--------------------------------------------------------------------------------

inputActionEscape :: Game ()
inputActionEscape = zoomInputState $ do
    hist .= empty
    mode %= escapeMode
    selectState .= Nothing
    where
    escapeMode = \case
        _ -> NormalMode

--------------------------------------------------------------------------------

toggleViewPanel :: PanelName -> Game ()
toggleViewPanel x = zoomInputState $ visiblePanels %= toggleSet x

isPanelVisible :: PanelName -> Game Bool
isPanelVisible pname = zoomInputState $ uses visiblePanels (Set.member pname)

--------------------------------------------------------------------------------

