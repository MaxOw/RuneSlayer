module Tutorial
    ( defaultTutorialState
    , restart, skipAll
    , update
    , display
    , inputActionHook
    , entityActionsHook
    ) where

import Delude
import qualified Data.Set as Set
import Engine.Layout.Alt (Layout)

import Engine (userState, EngineState)
import Types (Game, St)
import Types.Entity (DirectedEntityAction)
import Types.InputState (InputMode(..))
import Types.InputAction
import Types.EntityAction
import Types.Entity.Common (defaultDelta, timeInSeconds, Duration)
import Types.Tutorial
import Tutorial.Layout (layout_tutorialPage)
import InputState.Actions (showActionKeySeqs, getMode)
import qualified Data.Timer as Timer
import Focus (focusEntityId)

-- TODO:
-- Add step about runes
-- Add step about attacking enemies...

--------------------------------------------------------------------------------

defaultTutorialState :: TutorialState
defaultTutorialState  = def

restart :: Game ()
restart = do
    tutorialState.currentStep .= TutorialStep_Start
    nextTutorialStep

skipAll :: Game ()
skipAll = tutorialState.currentStep .= TutorialStep_Done

update :: Game ()
update = do
    tutorialState.timer %= Timer.update defaultDelta
    handleTutorialStep =<< use (tutorialState.currentStep)

handleTutorialStep :: TutorialStep -> Game ()
handleTutorialStep = \case
    TutorialStep_Start          -> tutorialStart
    TutorialStep_Movement       -> tutorialMovement
    TutorialStep_PickingUpItems -> tutorialPickingUpItems
    TutorialStep_Interaction    -> tutorialInteraction
    TutorialStep_Inventory      -> tutorialInventory
    TutorialStep_Runes          -> tutorialRunes
    TutorialStep_Attack         -> tutorialAttack
    TutorialStep_Done           -> tutorialDone

tutorialStart :: Game ()
tutorialStart = whenM welcomeDialogDone nextTutorialStep
    where
    welcomeDialogDone = andM
        [ checkActivated InputAction_NextPage
        , isNormalMode ]

tutorialMovement :: Game ()
tutorialMovement = makeTransition checkStepDone pageDesc
    where
    checkStepDone = allM checkActivated $ map SimpleMove boundedRange
    pageDesc = do
        let titleText = "Movement."
        let dirKeys = showActionKeySeqs NormalMode . SimpleMove
        kup    <- dirKeys MoveUp
        kdown  <- dirKeys MoveDown
        kleft  <- dirKeys MoveLeft
        kright <- dirKeys MoveRight
        let contentText = mconcat
                [ "To move in cardinal directions press: "
                , kdown  , " - South, "
                , kup    , " - North, "
                , kleft  , " - West, "
                , kright , " - East"
                ]
        return $ def
            & title   .~ titleText
            & content .~ contentText

tutorialPickingUpItems :: Game ()
tutorialPickingUpItems = makeTransition checkStepDone pageDesc
    where
    checkStepDone = andM
        [ checkActivated PickupAllItems
        , currentStepSatisified ]
    pageDesc = do
        let titleText = "Picking things up."
        kpai <- showActionKeySeqs NormalMode PickupAllItems
        let contentText = mconcat
                [ "You can press ", kpai, " to pickup all nearby items. "
                , "Try moving near that bow over there and picking it up."
                ]
        return $ def
            & title   .~ titleText
            & content .~ contentText

tutorialInventory :: Game ()
tutorialInventory = makeTransition checkStepDone pageDesc
    where
    checkStepDone = andM
        [ checkActivated $ SetMode InventoryMode
        , checkActivated InputAction_Escape
        , isNormalMode ]
    pageDesc = do
        let titleText = "Inventory."
        kinv <- showActionKeySeqs NormalMode $ SetMode InventoryMode
        kesc <- showActionKeySeqs InventoryMode $ InputAction_Escape
        let contentText = mconcat
                [ "Press ", kinv, " to open inventory. "
                , "Pressing ", kesc, " should take you back out."
                ]
        return $ def
            & title   .~ titleText
            & content .~ contentText

tutorialInteraction :: Game ()
tutorialInteraction = makeTransition checkStepDone pageDesc
    where
    checkStepDone = andM
        [ checkActivated Interact
        , isNormalMode
        , currentStepSatisified ]
    pageDesc = do
        let titleText = "Interactions."
        ksac <- showActionKeySeqs NormalMode Interact
        let contentText = mconcat
                [ "Press ", ksac, " to interact with NPCs and the environment."
                , "Try striking a conversation with Bertram or inspecting "
                , "the contents of that chest."
                ]
        return $ def
            & title   .~ titleText
            & content .~ contentText

tutorialRunes :: Game ()
tutorialRunes = makeTransition checkStepDone pageDesc
    where
    checkStepDone = andM
        [ checkActivated StartRunicMode
        , isNormalMode
        , currentStepSatisified ]
    pageDesc = do
        let titleText = "Runes."
        krunic <- showActionKeySeqs NormalMode StartRunicMode
        -- kesc <- showActionKeySeqs RunicMode $ InputAction_Escape
        let contentText = mconcat
                [ "Press ", krunic, " to start runic mode. "
                , "By answering questions about the runes you've learned, "
                , "you can load runic points (RP) that you will need when "
                , "attacking/defending. Try loading some RP."
                ]
        return $ def
            & title   .~ titleText
            & content .~ contentText

tutorialAttack :: Game ()
tutorialAttack = makeTransition checkStepDone pageDesc
    where
    checkStepDone = andM
        [ checkActivated ExecuteAttack
        , currentStepSatisified ]
    pageDesc = do
        let titleText = "Attacking enemies."
        kattack <- showActionKeySeqs NormalMode ExecuteAttack
        let contentText = mconcat
                [ "Now assuming you have bow and quiver with arrows equipped "
                , "(or maybe some other weapon) and have enough runic points "
                , "loaded to be able to attack, you can move within attack "
                , "range of the enemy and press ", kattack, " to attack. "
                ]
        return $ def
            & title   .~ titleText
            & content .~ contentText

tutorialDone :: Game ()
tutorialDone = tutorialState.currentPage .= Nothing

--------------------------------------------------------------------------------

display :: Game (Maybe Layout)
display = return . fmap layout_tutorialPage =<< use (tutorialState.currentPage)

--------------------------------------------------------------------------------

inputActionHook :: InputAction -> Game ()
inputActionHook x = unlessDone
    $ tutorialState.activatedInputActions %= Set.insert x

entityActionsHook :: [DirectedEntityAction] -> Game ()
entityActionsHook = unlessDone . mapM_ handleAction

handleAction :: DirectedEntityAction -> Game ()
handleAction (DirectedEntityAction _ act) = case act of
    EntityAction_SelfPassTo   p _ -> satisfyFor p TutorialStep_PickingUpItems
    EntityAction_Interact      {} -> satisfy      TutorialStep_Interaction
    EntityAction_SelfAttacked _ p -> satisfyFor p TutorialStep_Attack
    EntityAction_PlayerAction (PlayerAction_UpdateRune _) -> satisfyRunes
    _ -> return ()
    where
    satisfyRunes = satisfy TutorialStep_Runes
    satisfyFor p x = whenFocus p $ satisfy x
    whenFocus p = whenM ((p ==) <$> focusEntityId)
    satisfy x = tutorialState.satisfied %= Set.insert x

unlessDone :: Game () -> Game ()
unlessDone = unlessM (uses (tutorialState.currentStep) (== TutorialStep_Done))

makeTransition :: Game Bool -> Game TutorialPage -> Game ()
makeTransition nextStepCondition pageDescription = do
    tutorialState.currentPage .= Nothing
    whenTimerUp TimerType_PostPageDelay $ do
        assign (tutorialState.currentPage) . Just =<< pageDescription
        whenM nextStepCondition nextTutorialStep
--      whenTimerUp TimerType_MinPageTime $ whenM condOrTimer nextTutorialStep
--  where
--  condOrTimer = orM [ nextStepCondition, isTimerUp TimerType_MaxPageTime ]

currentStepSatisified :: Game Bool
currentStepSatisified
    = uses (tutorialState.satisfied) . Set.member =<< getCurrentStep

isTimerUp :: TimerType -> Game Bool
isTimerUp = uses (tutorialState.timer) . Timer.isTimerUp

startTimer :: TimerType -> Duration -> Game ()
startTimer tt d = tutorialState.timer %= Timer.start tt d

whenTimerUp :: TimerType -> Game () -> Game ()
whenTimerUp tt = whenM (isTimerUp tt)

checkActivated :: InputAction -> Game Bool
checkActivated = uses (tutorialState.activatedInputActions) . Set.member

nextTutorialStep :: Game ()
nextTutorialStep = do
    cs <- use $ tutorialState.currentStep
    tutorialState .= defaultTutorialState
    startTimer TimerType_PostPageDelay $ timeInSeconds  0.5
 -- startTimer TimerType_MinPageTime   $ timeInSeconds  3.5
 -- startTimer TimerType_MaxPageTime   $ timeInSeconds 10.5
    tutorialState.currentStep .= next cs

getCurrentStep :: Game TutorialStep
getCurrentStep = use (tutorialState.currentStep)

-- getPreviousStep :: Game TutorialStep
-- getPreviousStep = uses (tutorialState.currentStep) precStop

tutorialState :: Lens' (EngineState St) TutorialState
tutorialState = userState.gameState.ff#tutorialState

isNormalMode :: Game Bool
isNormalMode = (NormalMode ==) <$> getMode
