{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Controller
    ( handleEvent
    ) where

import Delude
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Map as PrefixMap
import qualified Engine
import Engine (userState)
import Engine.Events.Types hiding (Event)

import Types (Game, Event)
import Types.Entity
import Types.Entity.Common (EntityId)
import Types.St
import Types.MenuState
import InputState
import GameState
import Focus

--------------------------------------------------------------------------------

handleEvent :: Event -> Game ()
handleEvent event = use (userState.menuState) >>= \case
    MainMenu -> handleMenu event
    InGame   -> handleGame event

handleMenu :: Event -> Game ()
-- handleMenu event = defaultExit event
handleMenu = interpretGameEvent

handleGame :: Event -> Game ()
handleGame = interpretGameEvent

--------------------------------------------------------------------------------

interpretGameEvent :: Event -> Game ()
interpretGameEvent (EventKey key _ keyState mods) = case keyState of
    KeyState'Pressed   -> handleKeyPressed  keypress
    KeyState'Released  -> handleKeyReleased keypress
    KeyState'Repeating -> return ()
    where keypress = Keypress key mods
interpretGameEvent _ = return ()

handleKeyPressed :: Keypress -> Game ()
handleKeyPressed kp = do
    print kp
    mSelectState <- getSelectState
    case mSelectState of
        Just sm -> handleSelectMode sm kp
        Nothing -> handleOtherModes kp

handleSelectMode :: SelectState -> Keypress -> Game ()
handleSelectMode s kp = do
    case keyToChar $ keypressKey kp of
        Nothing -> case keypressKey kp of
            -- TODO: This shouldn't be hardcoded...
            Key'Backspace -> backspaceSelect
            _             -> endSelect
        Just ch -> do
            let smap = s^.selectMap
            selseq <- appendSelect ch
            let selectHandler f v = selectLookup f selseq smap v
            case s^.selectKind of
                SelectPickup v -> selectHandler pickupItem v
                SelectDrop   v -> selectHandler   dropItem v
            unless (isPartialMatch smap selseq) endSelect

selectLookup :: (a -> Game ()) -> Seq Char -> SelectMap -> Vector a -> Game ()
selectLookup f sq m v = case PrefixMap.lookup (toList sq) m of
    Nothing -> return ()
    Just i -> case Vector.indexM v i of
        Nothing -> return ()
        Just a  -> f a

handleOtherModes :: Keypress -> Game ()
handleOtherModes kp = do
    -- putStrLn $ show (keypressKey kp) ++ " pressed"
    -- print =<< use (userState.inputState)
    -- Add keypress to history
    keyseq <- appendHist kp
    -- Mach sequence against keymap
    -- print $ fmap keypressKey keyseq
    print $ toList keyseq
    mAction <- matchKeymap keyseq
    print $ mAction
    whenJust mAction $ \act -> do
        handleActivation act
        registerDeactivation kp act

-- TODO: Remember to clear all keypress deactivators on focus lost

handleKeyReleased :: Keypress -> Game ()
handleKeyReleased = mapM_ handleDeactivation <=< popDeactivators

--------------------------------------------------------------------------------

handleActivation :: InputAction -> Game ()
handleActivation = \case
    SimpleMove       d -> activateAction (ActiveMove d)
    SetMode          m -> setMode m
    ToggleDebug      f -> toggleDebug f
    ToggleViewPanel  p -> toggleViewPanel p
    PickupAllItems     -> pickupAllItems
    DropAllItems       -> dropAllItems
    SelectItemToPickUp -> selectItemToPickUp
    SelectItemToDrop   -> selectItemToDrop
    InputAction_Escape -> inputActionEscape
    FastQuit           -> Engine.closeWindow

handleDeactivation :: InputAction -> Game ()
handleDeactivation = \case
    SimpleMove d -> deactivateAction (ActiveMove d)
    _            -> return ()

--------------------------------------------------------------------------------

defaultExit :: Event -> Game ()
defaultExit = \case
    EventKey Key'Q _ _ _ -> Engine.closeWindow
    _                    -> return ()

--------------------------------------------------------------------------------

toggleDebug :: DebugFlag -> Game ()
toggleDebug = actOnFocusedEntity . EntityAction_ToggleDebug

pickupItem :: EntityId -> Game ()
pickupItem eid = withFocusId $ actOnEntity eid . EntityAction_SelfPickupBy

pickupAllItems :: Game ()
pickupAllItems = withFocusId $ \fi -> do
    es <- fmap (view entityId) <$> liftGame focusItemsInRange
    mapM_ (flip actOnEntity $ EntityAction_SelfPickupBy fi) es

dropAllItems :: Game ()
dropAllItems = actOnFocusedEntity EntityAction_DropAllItems

dropItem :: EntityId -> Game ()
dropItem = actOnFocusedEntity . EntityAction_DropItem

selectItemToPickUp :: Game ()
selectItemToPickUp = do
    es <- fmap (view entityId) <$> liftGame focusItemsInRange
    case es of
        []   -> return ()
        e:[] -> pickupItem e
        _    -> do
            -- setMode $ StatusMode Inventory
            let (sv, sm)  = makeSelectMap es
            print sm
            startSelect (SelectPickup sv) sm

selectItemToDrop :: Game ()
selectItemToDrop = do
    es <- fmap (view entityId) <$> liftGame focusItemsInInventory
    case es of
        []   -> return ()
        e:[] -> dropItem e
        _    -> do
            -- setMode $ StatusMode Inventory
            let (sv, sm)  = makeSelectMap es
            print sm
            startSelect (SelectDrop sv) sm

makeSelectMap :: [a] -> (Vector a, SelectMap)
makeSelectMap es = (Vector.fromList es, selMap)
    where
    selMap = PrefixMap.fromList $ zip ps $ map fst $ zip [0..] es
    ps = allSelectors prefixDepth
    prefixDepth :: Int
    prefixDepth = ceiling @Float
        $ logBase (genericLength defaultSelectors) (genericLength es)

allSelectors :: Int -> [String]
allSelectors x = if x <= 0 then [] else go x
    where go i = if i <= 0 then [[]] else (:) <$> defaultSelectors <*> go (i-1)
