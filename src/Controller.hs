{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Controller
    ( handleEvent
    ) where

import Delude
import qualified Engine
import Engine (userState)
import Engine.Events.Types hiding (Event)

import Types (Game, Event)
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
handleSelectMode s kp = case keypressKey kp of
    k | Just ch <- keyToChar k -> handleSelectKind s =<< appendSelect ch
    Key'Backspace -> backspaceSelect -- TODO: This shouldn't be hardcoded...
    _             -> endSelect

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
        handleActionFinalizers act
        handleActivation act
        registerDeactivation kp act

-- TODO: Remember to clear all keypress deactivators on focus lost

handleKeyReleased :: Keypress -> Game ()
handleKeyReleased = mapM_ handleDeactivation <=< popDeactivators

--------------------------------------------------------------------------------

handleActivation :: InputAction -> Game ()
handleActivation = \case
    SimpleMove        d -> activateAction (ActiveMove d)
    SetMode           m -> setMode m
    ToggleDebug       f -> toggleDebug f
    DebugRunAnimation k -> debugRunAnimation k
    ToggleViewPanel   p -> toggleViewPanel p
    PickupAllItems      -> pickupAllItems
    DropAllItems        -> dropAllItems
    ExecuteAttack       -> executeAttack
    SelectItemToPickUp  -> selectItemToPickUp
    SelectItemToDrop    -> selectItemToDrop
    SelectItemToFocus   -> selectItemToFocus
    InputAction_Escape  -> inputActionEscape
    FastQuit            -> Engine.closeWindow

handleDeactivation :: InputAction -> Game ()
handleDeactivation = \case
    SimpleMove d -> deactivateAction (ActiveMove d)
    _            -> return ()

-- Some actions need to be finalized when certain actions are started
-- We do that here:
handleActionFinalizers :: InputAction -> Game ()
handleActionFinalizers act = sequence_ $ map ($ act)
    [ finalize_selectItemToFocus
    ]

finalize_selectItemToFocus :: InputAction -> Game ()
finalize_selectItemToFocus = \case
    SelectItemToPickUp -> return ()
    SelectItemToDrop   -> return ()
    SelectItemToFocus  -> return ()
    _ -> unfocusItem

--------------------------------------------------------------------------------

defaultExit :: Event -> Game ()
defaultExit = \case
    EventKey Key'Q _ _ _ -> Engine.closeWindow
    _                    -> return ()

--------------------------------------------------------------------------------

selectItemToPickUp :: Game ()
selectItemToPickUp = do
    es <- fmap (view entityId) <$> focusItemsInRange
    startSelect SelectPickup es

selectItemToDrop :: Game ()
selectItemToDrop = do
    es <- fmap (view entityId) <$> focusItemsInInventory
    startSelect SelectDrop es

selectItemToFocus :: Game ()
selectItemToFocus = do
    res <- fmap (view entityId) <$> focusItemsInRange
    ies <- fmap (view entityId) <$> focusItemsInInventory
    startSelect SelectFocus $ res <> ies

