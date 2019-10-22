module Tutorial
    ( defaultTutorialState
    , restart, skipAll
    , update
    , handleActivation
    , display
    , inputActionHook
    , entityActionsHook
    ) where

import Delude
import qualified Data.Set as Set
import Engine.Layout.Alt (Layout)

import Engine (EngineState)
import Types (Game, St)
import Types.Config (ConfigDebugFlag(..))
import Types.Entity (DirectedEntityAction)
import Types.GameState (gameState)
import Types.InputState (InputMode(..))
import Types.InputAction
import Types.EntityAction
import Types.Entity.Common (defaultDelta, timeInSeconds, Duration)
import Types.Tutorial
import Types.Entity.Agent hiding (TimerType)
import Entity
import Tutorial.Layout (layout_tutorialPage)
import InputState.Actions (showActionKeySeqs, getMode)
import GameState.Query (lookupEntity, isConfigDebugFlagOn)
import qualified Data.Timer as Timer
import Focus (focusEntityId)

--------------------------------------------------------------------------------

defaultTutorialState :: TutorialState
defaultTutorialState = def

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

handleActivation :: TutorialAction -> Game ()
handleActivation = \case
    TutorialAction_SkipAll -> skipAll
    TutorialAction_Restart -> restart

handleTutorialStep :: TutorialStep -> Game ()
handleTutorialStep = \case
    TutorialStep_Start          -> tutorialStart
    TutorialStep_Movement       -> tutorialMovement
    TutorialStep_PickingUpItems -> tutorialPickingUpItems
    TutorialStep_Interaction    -> tutorialInteraction
    TutorialStep_Inventory      -> tutorialInventory
    TutorialStep_RunesStatus    -> tutorialRunesStatus
    TutorialStep_RunicMode      -> tutorialRunicMode
    TutorialStep_Attack         -> tutorialAttack
    TutorialStep_Done           -> tutorialDone

tutorialStart :: Game ()
tutorialStart = whenM welcomeDialogDone nextTutorialStep
    where
    welcomeDialogDone = andM
        [ orM [ checkActivated InputAction_NextPage
              , isConfigDebugFlagOn ConfigDebugFlag_NoStory ]
        , isNormalMode ]

keyActivatedPart :: InputMode -> InputAction -> Game ContentPart
keyActivatedPart m k = do
    kt <- showActionKeySeqs m k
    ks <- checkActivated k
    return $ ckeys ks kt

makeTaskPart :: TutorialReq -> Game (Text -> ContentPart)
makeTaskPart req = ctask <$> isSatisfied req

tutorialMovement :: Game ()
tutorialMovement = makeTransition checkStepDone pageDesc
    where
    checkStepDone = allM checkActivated $ map SimpleMove boundedRange
    pageDesc = do
        let titleText = "Movement."
        let dirKeys = keyActivatedPart NormalMode . SimpleMove
        kup    <- dirKeys MoveUp
        kdown  <- dirKeys MoveDown
        kleft  <- dirKeys MoveLeft
        kright <- dirKeys MoveRight
        let contentText =
                [ "Hello. This is a little tutorial to ease you "
                , "into the controls better, since they are a bit unusual "
                , "(they're based on vi). Let's start with movement, "
                , "to move in cardinal directions press: "
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
        , isSatisfied TutorialReq_PickUpItem ]
    pageDesc = do
        let titleText = "Picking things up."
        kpai <- keyActivatedPart NormalMode PickupAllItems
        f <- makeTaskPart TutorialReq_PickUpItem
        let contentText =
                [ "You can press ", kpai, " to pickup all nearby items. "
                , f "Try moving near that bow over there and picking it up."
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
        kinv <- keyActivatedPart NormalMode $ SetMode InventoryMode
        kesc <- keyActivatedPart InventoryMode $ InputAction_Escape
        let contentText =
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
        , checkActivated InputAction_Escape
        , isNormalMode
        , isSatisfied TutorialReq_TalkToBertram
        , isSatisfied TutorialReq_InspectChest ]
    pageDesc = do
        let titleText = "Interactions."
        ksac <- keyActivatedPart NormalMode Interact
        fb <- makeTaskPart TutorialReq_TalkToBertram
        fc <- makeTaskPart TutorialReq_InspectChest
        let contentText =
                [ "Press ", ksac, " to interact with NPCs and the environment."
                , fb "Try striking a conversation with Bertram",  "and"
                , fc "inspecting the contents of that chest."
                ]
        return $ def
            & title   .~ titleText
            & content .~ contentText

tutorialRunesStatus :: Game ()
tutorialRunesStatus = makeTransition checkStepDone pageDesc
    where
    checkStepDone = andM
        [ checkActivated $ SetMode RunicStatusMode
        , isNormalMode ]
    pageDesc = do
        let titleText = "Runes (Status)."
        krunic <- keyActivatedPart NormalMode $ SetMode RunicStatusMode
        kesc <- keyActivatedPart RunicStatusMode $ InputAction_Escape
        let contentText =
                [ "Press ", krunic, " to show you the status of all runes "
                , "you've learned so far."
                , "Pressing ", kesc, " should take you back out."
                ]
        return $ def
            & title   .~ titleText
            & content .~ contentText

tutorialRunicMode :: Game ()
tutorialRunicMode = makeTransition checkStepDone pageDesc
    where
    checkStepDone = andM
        [ checkActivated StartRunicMode
        , isNormalMode
        , isSatisfied TutorialReq_LoadRune ]
    pageDesc = do
        let titleText = "Runic Mode."
        krunic <- keyActivatedPart NormalMode StartRunicMode
        f <- makeTaskPart TutorialReq_LoadRune
        let contentText =
                [ "Press ", krunic, " to start runic mode. "
                , "By answering questions about the runes you've learned, "
                , "you can load runic points (RP) that you will need when "
                , "attacking/defending. ", f "Try loading some RP."
                ]
        return $ def
            & title   .~ titleText
            & content .~ contentText

tutorialAttack :: Game ()
tutorialAttack = makeTransition checkStepDone pageDesc
    where
    checkStepDone = andM
        [ checkActivated ExecuteAttack
        , isSatisfied TutorialReq_Attack ]
    pageDesc = do
        let titleText = "Attacking enemies."
        kattack <- keyActivatedPart NormalMode ExecuteAttack
        f <- makeTaskPart TutorialReq_Attack
        let contentText =
                [ "Now assuming you have bow and quiver with arrows equipped "
                , "(or maybe some other weapon) and have enough runic points "
                , "loaded to be able to attack, you can move within attack "
                , "range to the enemy and press ", kattack, " to ", f "attack. "
                ]
        return $ def
            & title   .~ titleText
            & content .~ contentText

tutorialDone :: Game ()
tutorialDone = tutorialState.currentPage .= Nothing

--------------------------------------------------------------------------------

ckeys :: Bool -> Text -> ContentPart
ckeys s x = def
    & ff#contentType .~ ContentType_Keys
    & ff#satisfied   .~ s
    & ff#text        .~ x

ctask :: Bool -> Text -> ContentPart
ctask s x = def
    & ff#contentType .~ ContentType_Task
    & ff#satisfied   .~ s
    & ff#text        .~ x

display :: Game (Maybe Layout)
display = return . fmap layout_tutorialPage =<< use (tutorialState.currentPage)

--------------------------------------------------------------------------------

inputActionHook :: InputAction -> Game ()
inputActionHook x = unlessDone
    $ tutorialState.activatedInputActions %= Set.insert x

entityActionsHook :: [DirectedEntityAction] -> Game ()
entityActionsHook = unlessDone . mapM_ handleAction

handleAction :: DirectedEntityAction -> Game ()
handleAction (DirectedEntityAction eid act) = case act of
    EntityAction_SelfPassTo   p _ -> satisfyPickUpItem p
    EntityAction_Interact      {} -> satisfyInteract
    EntityAction_SelfAttacked _ p -> satisfyAttack p
    EntityAction_PlayerAction (PlayerAction_UpdateRune _ True) -> satisfyLoadRune
    _ -> return ()
    where
    satisfyPickUpItem p = whenFocus p $ satisfy TutorialReq_PickUpItem
    satisfyLoadRune = satisfy TutorialReq_LoadRune
    satisfyAttack p = whenFocus p $ satisfy TutorialReq_Attack

    satisfyInteract = do
        me <- lookupEntity eid
        whenJust me $ \e -> do
            let ak    = e^?entity.oracleAgentType.traverse.agentKind
            let isNPC = ak == Just AgentKind_NPC
            let pk    = e^?entity.oracleContent

            if | isNPC     -> satisfy TutorialReq_TalkToBertram
               | isJust pk -> satisfy TutorialReq_InspectChest
               | otherwise -> return ()

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

isSatisfied :: TutorialReq -> Game Bool
isSatisfied = uses (tutorialState.satisfied) . Set.member

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

-- getCurrentStep :: Game TutorialStep
-- getCurrentStep = use (tutorialState.currentStep)

-- getPreviousStep :: Game TutorialStep
-- getPreviousStep = uses (tutorialState.currentStep) precStop

tutorialState :: Lens' (EngineState St) TutorialState
tutorialState = gameState.ff#tutorialState

isNormalMode :: Game Bool
isNormalMode = (NormalMode ==) <$> getMode
