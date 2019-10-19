module Story
    ( StoryState
    , init
    , update
    , registerNPC
    , startDialog
    , nextPage
    , noStory
    , display
    ) where

import Delude hiding (init)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Zipper as Zipper

import Engine (EngineState)
import Engine.Layout.Alt (Layout)
import Types (Game, St)
import Types.Entity.Common
import Types.Entity.Agent (ScriptName(..))
import Types.GameState (gameState)
import Types.Story
import Types.EntityAction
import Types.Entity
import Types.InputAction
import Types.Entity.Animation (Direction (..))
import GameState.Query (lookupEntity)
import GameState.Actions (actOnEntity, actOnFocusedEntity)
import Entity
import qualified Data.Timer as Timer
import InputState.Actions
import Dhall.Utils (inputAuto)
import GUI.Layout (layout_storyDialog)

--------------------------------------------------------------------------------

init :: MonadIO m => m StoryState
init = do
    ss <- inputAuto "data/desc" "./StoryDialog.dhall"
    return $ StoryState
        { field_currentStep   = StoryStep_Start
        , field_started       = mempty
        , field_storyDialog   = def
        , field_register      = def
        , field_bertramState  = BertramState_Welcome
        , field_dialogMap     = ss
        , field_timer         = def
        }

update :: Game ()
update = do
    storyState.timer %= Timer.update defaultDelta
    handleStoryStep =<< use (storyState.currentStep)

handleStoryStep :: StoryStep -> Game ()
handleStoryStep = \case
    StoryStep_Start   -> storyStart
    StoryStep_Welcome -> storyWelcome
    StoryStep_Done    -> return ()

storyStart :: Game ()
storyStart = makeStoryStep StoryStep_Start startStep updateStep endStep
    where
    startStep  = do
        actOnFocusedEntity $ EntityAction_SetValue $ EntityValue_Direction North
        startTimer TimerType_Delay 0.3
    updateStep = isTimerUp  TimerType_Delay
    endStep    = return ()

storyWelcome :: Game ()
storyWelcome = makeStoryStep StoryStep_Welcome startStep updateStep endStep
    where
    targetLocation = locM 1.2 1.9
    startStep = do
        whenJustM (lookupRegister ScriptName_Bertram) $ \eid ->
            actOnEntity eid $ EntityAction_MoveTo targetLocation
        return ()

    updateStep = do
        -- playerLocation <- fromMaybe (locM 0 0) <$> focusLocation
        berLocation <- fromMaybe (locM 0 0) <$> bertramLocation
        if calcDistance berLocation targetLocation <= 0.1
        then startWelcomeDialog >> return True
        else return False

    bertramLocation = runMaybeT $ do
        bid <- MaybeT $ lookupRegister ScriptName_Bertram
        ber <- MaybeT $ lookupEntity bid
        MaybeT $ pure $ ber^.entity.oracleLocation

    endStep = return ()

    startWelcomeDialog = do
        whenJustM (lookupRegister ScriptName_Bertram) startDialog

registerNPC :: ScriptName -> EntityId -> Game ()
registerNPC k v = storyState.register %= Map.insert k v

startDialog :: EntityId -> Game ()
startDialog eid = whenJustM (getStoryDialogFor eid) $ \sd -> do
    setMode StoryDialogMode
    storyState.storyDialog .= Just sd

getStoryDialogFor :: EntityId -> Game (Maybe StoryDialog)
getStoryDialogFor eid = runMaybeT $ do
    e <- MaybeT $ lookupEntity eid
    let getName = e^?entity.oracleAgentType.traverse.ff#scriptName.traverse
    sn <- MaybeT $ pure getName
    dm <- MaybeT $ uses (storyState.ff#dialogMap) Just
    MaybeT $ forScript e dm sn

forScript :: EntityWithId -> DialogMap -> ScriptName -> Game (Maybe StoryDialog)
forScript e dm sn = case sn of
    ScriptName_Bertram -> bertramDialog
    ScriptName_Default -> return Nothing
    where
    bertramDialog = use (storyState.bertramState) >>= \case
        BertramState_Welcome -> mkb $ ff#welcome
        BertramState_Waiting -> mkb $ ff#waiting

    mkb pages = return $ Just $ StoryDialog
        { field_title       = "Old Man Bertram" -- get entity display name
        , field_entityId    = e^.entityId
        , field_scriptName  = sn
        , field_dialogPages = Zipper.fromList $ dm^.ff#bertram.pages
        }

nextPage :: Game ()
nextPage = whenJustM (use $ storyState.storyDialog) $ \sd ->
    nextPageFor (sd^.entityId) (sd^.ff#scriptName)

nextPageFor :: EntityId -> ScriptName -> Game ()
nextPageFor eid = \case
    ScriptName_Bertram -> bertramNextPage
    ScriptName_Default -> return ()
    where
    bertramNextPage = use (storyState.bertramState) >>= \case
        BertramState_Welcome -> nxb False BertramState_Waiting
        BertramState_Waiting -> nxb False BertramState_Waiting

    nxb displayNextState n = do
        storyState.bertramState .= n
        dp <- preuse $ storyState.storyDialog.traverse.ff#dialogPages
        if fmap Zipper.isRightmost dp == Just False
        then storyState.storyDialog.traverse.ff#dialogPages %= Zipper.right
        else if displayNextState
            then startDialog eid
            else storyState.storyDialog .= Nothing >> inputActionEscape

{-
    whenJustM queryPlayer $ \p -> do
        rs <- use $ context.resources.runeSet
        addRunes p $ runeSetByLevel 1 rs
-}

makeStoryStep :: StoryStep -> Game () -> Game Bool -> Game () -> Game ()
makeStoryStep step startStep updateStep endStep = do
    uses (storyState.ff#started) (Set.member step) >>= \case
        False -> startStep >> storyState.ff#started %= Set.insert step
        True  -> whenM updateStep (endStep >> nextStoryStep)

nextStoryStep :: Game ()
nextStoryStep = storyState.currentStep %= nextStop

noStory :: Game ()
noStory = do
    storyState.currentStep .= StoryStep_Done
    inputActionEscape

display :: Game Layout
display = do
    msd <- use $ storyState.storyDialog
    ksq <- showActionKeySeqs StoryDialogMode InputAction_NextPage
    case msd of
        Nothing -> return mempty
        Just sd -> return $ layout_storyDialog $ def
            & title   .~ sd^.title
            & content .~ fromMaybe "" (Zipper.focus $ sd^.ff#dialogPages)
            & ff#nextPageKey .~ ksq

startTimer :: TimerType -> Duration -> Game ()
startTimer tt d = storyState.timer %= Timer.start tt d

isTimerUp :: TimerType -> Game Bool
isTimerUp = uses (storyState.timer) . Timer.isTimerUp

--------------------------------------------------------------------------------

lookupRegister :: ScriptName -> Game (Maybe EntityId)
lookupRegister = uses (storyState.register) . Map.lookup

storyState :: Lens' (EngineState St) StoryState
storyState = gameState.ff#storyState

