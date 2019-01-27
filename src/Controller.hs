{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Controller
    ( handleEvent
    ) where

import Delude
import qualified Engine
import Engine (userState)
import Engine.Events.Types hiding (Event)

import Types (Game, Event)
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

selectItemToPickUp :: Game ()
selectItemToPickUp = do
    es <- fmap (view entityId) <$> liftGame focusItemsInRange
    startSelect SelectPickup es

selectItemToDrop :: Game ()
selectItemToDrop = do
    es <- fmap (view entityId) <$> liftGame focusItemsInInventory
    startSelect SelectDrop es

