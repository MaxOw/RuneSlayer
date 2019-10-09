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
import Types.Entity.Common (EntityStatus (..))
import Types.Debug (DebugFlag(..))
import Types.EntityAction
import Types.Equipment
import Equipment (contentList)
import GameState.Query (canFitIntoContainer)
import InputState
import InputKeymap (keyToChar)
import GameState
import Focus

import Entity

import qualified Tutorial
import qualified Messages
import qualified Runes
import qualified MapEditor

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
    KeyState'Released  -> handleKeyReleased key
    KeyState'Repeating -> return ()
    where keypress = Keypress key mods
interpretGameEvent _ = return ()

handleKeyPressed :: Keypress -> Game ()
handleKeyPressed kp = do
    print kp
    cm <- getMode
    mSelectState <- getSelectState
    case mSelectState of
        Just sm -> handleSelectMode sm kp
        Nothing -> handleOtherModes kp
    m <- getMode
    -- This is a bit ad hoc...
    when (cm /= m) $ do
        finalizeMode cm
        initializeMode m

finalizeMode :: InputMode -> Game ()
finalizeMode = \case
    MapEditorMode -> finMapEditor
    _             -> return ()
    where
    finMapEditor = actOnFocusedEntity $ EntityAction_SetValue
        $ EntityValue_UnsetStatus $ EntityStatus_Ignore

initializeMode :: InputMode -> Game ()
initializeMode = \case
    MapEditorMode -> iniMapEditor
    _             -> return ()
    where
    iniMapEditor = actOnFocusedEntity $ EntityAction_SetValue
        $ EntityValue_SetStatus $ EntityStatus_Ignore

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
            registerDeactivation (keypressKey kp) act
        Nothing -> customModeHandler kp =<< getMode

-- TODO: Remember to clear all keypress deactivators on focus lost

handleKeyReleased :: Key -> Game ()
handleKeyReleased = mapM_ handleDeactivation <=< popDeactivators

customModeHandler :: Keypress -> InputMode -> Game ()
customModeHandler kp = \case
    RunicMode -> Runes.inputHandler kp
    _ -> return ()

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
    SwapWeapon           -> swapWeapon
    SetAttackMode     m  -> setAttackMode m
    StartRunicMode       -> Runes.startRunicMode
    SelectItemToPickUp   -> selectItemToPickUp
    SelectItemMoveTarget -> selectItemMoveTarget
    SelectItemToDrop     -> selectItemToDrop
    SelectItemToFocus    -> selectItemToFocus
    UseFocusedItem       -> useFocusedItem
    SelectInteraction    -> selectInteraction
    Interact             -> interact
    FastQuit             -> Engine.closeWindow
    MapEditorAction    a -> MapEditor.handleActivation a
    TutorialAction     a -> Tutorial.handleActivation a
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
    , finalize_clearLastResult
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

finalize_clearLastResult :: InputAction -> Game ()
finalize_clearLastResult _ = Runes.clearLastResult

--------------------------------------------------------------------------------

defaultExit :: Event -> Game ()
defaultExit = \case
    EventKey Key'Q _ _ _ -> Engine.closeWindow
    _                    -> return ()

--------------------------------------------------------------------------------

selectItemToPickUp :: Game ()
selectItemToPickUp = do
    es <- fmap (view entityId) <$> focusItemsInRange
    cs <- fmap (view entityId) <$> focusItemsInContainer
    startSelect SelectKind_Pickup (es <> cs)

selectItemMoveTarget :: Game ()
selectItemMoveTarget = getFocusedItem >>= \case
    Nothing -> return () -- Messages.addInfo "No item selected!"
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
    Nothing -> Messages.addInfo "No item selected!"
    Just fi -> actOnFocusedEntity $ EntityAction_UseItem fi

selectInteraction :: Game ()
selectInteraction = do
    mar <- focusInteractionsInRange
    case over (traverse._1) (view entityId) mar of
        [] -> Messages.addInfo "There's nothing to interact with nearby."
        ar -> startSelect SelectKind_Action ar

interact :: Game ()
interact = focusNearestInteractionInRange >>= \case
    Nothing  -> Messages.addInfo "There's nothing to interact with nearby."
    Just (e,_) -> withFocusId $ actOnEntity e . EntityAction_Interact Nothing

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
pickupAllItems = withFocusEntityWithId $ \fe -> do
    es <- focusItemsInRange
    cs <- focusItemsInContainer
    case es <> cs of
        [] -> Messages.addInfo "No items nearby."
        is -> mapM_ pickupItem =<< filterFitItems fe is

dropAllItems :: Game ()
dropAllItems = do
    mf <- focusEntity
    mapM_ dropItem $ mf^.traverse.oracleEquipment.traverse.to contentList

executeAttack :: Game ()
executeAttack = actOnFocusedEntity EntityAction_ExecuteAttack

swapWeapon :: Game ()
swapWeapon = actOnFocusedEntity EntityAction_SwapWeapon

setAttackMode :: AttackMode -> Game ()
setAttackMode = actOnPlayer . PlayerAction_SetAttackMode
