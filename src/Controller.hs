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
import Types.Entity.Animation (AnimationKind)
import Types.Debug (DebugFlag(..))
import Types.EntityAction
import InputState
import GameState
import Focus

import Skills.Runes (getRuneByName, isCorrectAnswer)
import Entity

--------------------------------------------------------------------------------

handleEvent :: Event -> Game ()
handleEvent event = use (userState.menuState) >>= \case
    MainMenu -> handleMenu event
    InGame   -> handleGame event

handleMenu :: Event -> Game ()
-- handleMenu event = defaultExit event
handleMenu = handleGame

handleGame :: Event -> Game ()
handleGame e = getGameOverScreen >>= \case
    Nothing -> interpretGameEvent e
    Just s -> when (s^.ff#pressAnyKey) goToMainMenu

goToMainMenu :: Game ()
goToMainMenu = Engine.closeWindow -- TODO

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
    -- Add keypress to history
    keyseq <- appendHist kp
    -- Mach sequence against keymap
    print $ toList keyseq
    mAction <- matchKeymap keyseq
    print $ mAction
    case mAction of
        Just act -> do
            handleActionFinalizers act
            handleActivation act
            registerDeactivation kp act
        Nothing -> customModeHandler kp =<< getMode

-- TODO: Remember to clear all keypress deactivators on focus lost

handleKeyReleased :: Keypress -> Game ()
handleKeyReleased = mapM_ handleDeactivation <=< popDeactivators

customModeHandler :: Keypress -> InputMode -> Game ()
customModeHandler kp = \case
    OffensiveMode -> customModeHandler_runicMode RuneType_Offensive kp
    DefensiveMode -> customModeHandler_runicMode RuneType_Defensive kp
    _ -> return ()

customModeHandler_runicMode :: RuneType -> Keypress -> Game ()
customModeHandler_runicMode rt kp = case keypressKey kp of
    k | Just ch <- keyToChar k -> appendInputString ch >> autoAccept
    Key'Backspace -> backspaceInputString
    Key'Enter     -> acceptAnswer
    Key'Space     -> acceptAnswer
    _             -> return ()
    where
    autoAccept = whenM verifyAnswer correctAnswer

    acceptAnswer = do
        ver <- verifyAnswer
        if ver then correctAnswer else wrongAnswer

    verifyAnswer = do
        ans <- getInputString
        mfo <- focusEntity
        case flip entityOracle EntityQuery_PlayerStatus =<< mfo of
            Nothing -> return False
            Just ps -> do
                let mr = flip getRuneByName (ps^.ff#runicLevel)
                        =<< ps^.ff#selectedRune
                return $ fromMaybe False $ isCorrectAnswer ans <$> mr

    correctAnswer = do
        actOnPlayer $ PlayerAction_UpdateRune rt True
        inputActionEscape

    wrongAnswer   = do
        actOnPlayer $ PlayerAction_UpdateRune rt False
        inputActionEscape

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
    SetAttackMode     m -> setAttackMode m
    StartOffensiveMode  -> startOffensiveMode
    StartDefensiveMode  -> startDefensiveMode
    SelectItemToPickUp  -> selectItemToPickUp
    SelectItemToDrop    -> selectItemToDrop
    SelectItemToFocus   -> selectItemToFocus
    UseFocusedItem      -> useFocusedItem
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
    , finalize_clearInputString
    ]

finalize_selectItemToFocus :: InputAction -> Game ()
finalize_selectItemToFocus = \case
    SelectItemToPickUp -> return ()
    SelectItemToDrop   -> return ()
    SelectItemToFocus  -> return ()
    UseFocusedItem     -> return ()
    _ -> unfocusItem

finalize_clearInputString :: InputAction -> Game ()
finalize_clearInputString _ = clearInputString

--------------------------------------------------------------------------------

defaultExit :: Event -> Game ()
defaultExit = \case
    EventKey Key'Q _ _ _ -> Engine.closeWindow
    _                    -> return ()

--------------------------------------------------------------------------------

startOffensiveMode :: Game ()
startOffensiveMode = do
    actOnPlayer PlayerAction_SelectRune
    setMode OffensiveMode

startDefensiveMode :: Game ()
startDefensiveMode = do
    actOnPlayer PlayerAction_SelectRune
    setMode DefensiveMode

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

useFocusedItem :: Game ()
useFocusedItem = getFocusedItem >>= \case
    Nothing -> putStrLn "No item selected!" -- systemMessage "No item selected!"
    Just fi -> actOnFocusedEntity $ EntityAction_UseItem fi

--------------------------------------------------------------------------------

toggleDebug :: DebugFlag -> Game ()
toggleDebug x = do
    userState.debugFlags %= toggleSet x
    case x of
      DebugFlag_DrawPickupRange -> debugFocus EntityDebugFlag_DrawPickupRange
      _ -> return ()
    where
    debugFocus = actOnFocusedEntity . EntityAction_ToggleDebug

debugRunAnimation :: AnimationKind -> Game ()
debugRunAnimation = actOnFocusedEntity . EntityAction_RunAnimation

pickupAllItems :: Game ()
pickupAllItems = withFocusId $ \fi -> do
    es <- fmap (view entityId) <$> focusItemsInRange
    mapM_ (flip actOnEntity $ EntityAction_SelfAddedBy fi) es

dropAllItems :: Game ()
dropAllItems = actOnFocusedEntity EntityAction_DropAllItems

executeAttack :: Game ()
executeAttack = actOnFocusedEntity EntityAction_ExecuteAttack

setAttackMode :: AttackMode -> Game ()
setAttackMode = actOnPlayer . PlayerAction_SetAttackMode
