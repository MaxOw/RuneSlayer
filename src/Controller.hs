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
import Types.Equipment
import Types.Entity.Common (calcDistance)
import Equipment (contentList)
import GameState.Query (canFitIntoContainer)
import InputState
import InputKeymap (keyToChar)
import GameState
import GameState.Query
import Focus

import Skills.Runes (getRuneByName, isCorrectAnswer)
import Entity

import qualified Tutorial
import qualified Messages

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
            Tutorial.inputActionHook act
            handleActivation act
            registerDeactivation kp act
        Nothing -> customModeHandler kp =<< getMode

-- TODO: Remember to clear all keypress deactivators on focus lost

handleKeyReleased :: Keypress -> Game ()
handleKeyReleased = mapM_ handleDeactivation <=< popDeactivators

customModeHandler :: Keypress -> InputMode -> Game ()
customModeHandler kp = \case
    RunicMode -> customModeHandler_runicMode kp
    _ -> return ()

customModeHandler_runicMode :: Keypress -> Game ()
customModeHandler_runicMode kp = case keypressKey kp of
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
        actOnPlayer $ PlayerAction_UpdateRune True
        inputActionEscape

    wrongAnswer   = do
        actOnPlayer $ PlayerAction_UpdateRune False
        inputActionEscape

--------------------------------------------------------------------------------

handleActivation :: InputAction -> Game ()
handleActivation = \case
    SimpleMove        d  -> activateAction (ActiveMove d)
    SetMode           m  -> setMode m
    ToggleDebug       f  -> toggleDebug f
    DebugRunAnimation k  -> debugRunAnimation k
    ToggleViewPanel   p  -> toggleViewPanel p
    PickupAllItems       -> pickupAllItems
    DropAllItems         -> dropAllItems
    ExecuteAttack        -> executeAttack
    SetAttackMode     m  -> setAttackMode m
    StartRunicMode       -> startRunicMode
    SelectItemToPickUp   -> selectItemToPickUp
    SelectItemMoveTarget -> selectItemMoveTarget
    SelectItemToDrop     -> selectItemToDrop
    SelectItemToFocus    -> selectItemToFocus
    UseFocusedItem       -> useFocusedItem
    SelectInteraction    -> selectInteraction
    Interact             -> interact
    TalkToNPC            -> talkToNPC
    FastQuit             -> Engine.closeWindow
    InputAction_NextPage -> nextPage
    InputAction_Escape   -> inputActionEscape
    InputAction_Nothing  -> return ()

handleDeactivation :: InputAction -> Game ()
handleDeactivation = \case
    SimpleMove d -> deactivateAction (ActiveMove d)
    _            -> return ()

-- Some actions need to be finalized when certain actions are started
-- We do that here:
handleActionFinalizers :: InputAction -> Game ()
handleActionFinalizers act = sequence_ $ map ($ act)
    [ finalize_clearInventoryState
    , finalize_clearInputString
    ]

finalize_clearInventoryState :: InputAction -> Game ()
finalize_clearInventoryState = \case
    SelectItemToPickUp   -> return ()
    SelectItemToDrop     -> return ()
    SelectItemToFocus    -> return ()
    SelectItemMoveTarget -> return ()
    UseFocusedItem       -> return ()
    PickupAllItems       -> unfocusItem
    DropAllItems         -> unfocusItem
    _                    -> clearInventoryState

finalize_clearInputString :: InputAction -> Game ()
finalize_clearInputString _ = clearInputString

--------------------------------------------------------------------------------

defaultExit :: Event -> Game ()
defaultExit = \case
    EventKey Key'Q _ _ _ -> Engine.closeWindow
    _                    -> return ()

--------------------------------------------------------------------------------

startRunicMode :: Game ()
startRunicMode = do
    actOnPlayer PlayerAction_SelectRune
    setMode RunicMode

selectItemToPickUp :: Game ()
selectItemToPickUp = do
    es <- fmap (view entityId) <$> focusItemsInRange
    cs <- fmap (view entityId) <$> focusItemsInContainer
    startSelect SelectKind_Pickup (es <> cs)

selectItemMoveTarget :: Game ()
selectItemMoveTarget = getFocusedItem >>= \case
    Nothing -> return () -- Messages.add "No item selected!"
    Just fi -> lookupEntity fi >>= \case
        Nothing -> return ()
        Just fe -> startSelect SelectKind_MoveTo =<< getValidTargets fe
    where
    getValidTargets fe = do
        let fs = toList $ fe^.entity.oracleFittingSlots.traverse
        mb <- getBackpackTarget fe
        mc <- getContainerTarget fe
        -- TODO: remove current focus from the list of valid targets
        -- ct <- getCurrentTarget fe
        let dt = map ItemMoveTarget_EquipmentSlot fs
              <> catMaybes [mb, mc, Just ItemMoveTarget_Ground]
        return dt

    getBackpackTarget = fitForTarget
        (focusEquipmentSlot EquipmentSlot_Backpack)
        ItemMoveTarget_Backpack

    getContainerTarget = fitForTarget
        getInventoryContainer
        ItemMoveTarget_Container

    fitForTarget getT mt fe = getT >>= \case
        Nothing -> return Nothing
        Just bi -> bool Nothing (Just mt) <$> canFitIntoContainer fe bi

selectItemToDrop :: Game ()
selectItemToDrop = do
    es <- fmap (view entityId) <$> focusItemsInInventory
    cs <- fmap (view entityId) <$> focusItemsInContainer
    startSelect SelectKind_Drop (es <> cs)

selectItemToFocus :: Game ()
selectItemToFocus = do
    ies <- fmap (view entityId) <$> focusItemsInInventory
    ces <- fmap (view entityId) <$> focusItemsInContainer
    res <- fmap (view entityId) <$> focusItemsInRange
    startSelect SelectKind_Focus $ ies <> ces <> res

useFocusedItem :: Game ()
useFocusedItem = getFocusedItem >>= \case
    Nothing -> Messages.add "No item selected!"
    Just fi -> actOnFocusedEntity $ EntityAction_UseItem fi

selectInteraction :: Game ()
selectInteraction = do
    mar <- focusInteractionsInRange
    case over (traverse._1) (view entityId) mar of
        [] -> Messages.add "There's nothing to interact with nearby."
        ar -> startSelect SelectKind_Action ar

interact :: Game ()
interact = getNearest >>= \case
    Nothing  -> Messages.add "There's nothing to interact with nearby."
    Just eid -> withFocusId $ actOnEntity eid . EntityAction_Interact Nothing
    where
    getNearest = focusLocation >>= \case
        Nothing -> return Nothing
        Just lc -> do
            ns <- map (view _1) <$> focusInteractionsInRange
            let calcDist = fmap (calcDistance lc) . view (entity.oracleLocation)
            let eds = mapMaybe (\n -> (n,) <$> calcDist n) ns
            return $ viaNonEmpty head $ map fst $ sortWith snd eds

talkToNPC :: Game ()
talkToNPC = withFocusId $ \fid -> do
    -- TODO: rewrite this as interact parametrized with NPCs only.
    whenJustM (viaNonEmpty head <$> focusNPCsInRange) $ \eid -> do
        actOnEntity eid $ EntityAction_Interact Nothing fid

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
pickupAllItems = do
    es <- fmap (view entityId) <$> focusItemsInRange
    cs <- fmap (view entityId) <$> focusItemsInContainer
    case es <> cs of
        [] -> Messages.add "No items nearby."
        is -> mapM_ pickupItem is

dropAllItems :: Game ()
dropAllItems = do
    mf <- focusEntity
    mapM_ dropItem $ mf^.traverse.oracleEquipment.traverse.to contentList

executeAttack :: Game ()
executeAttack = actOnFocusedEntity EntityAction_ExecuteAttack

setAttackMode :: AttackMode -> Game ()
setAttackMode = actOnPlayer . PlayerAction_SetAttackMode
