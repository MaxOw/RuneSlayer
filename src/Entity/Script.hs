module Entity.Script where

import Delude
import qualified Data.HashMap.Strict as HashMap

import Types.Entity.Agent
import Types.Entity.Script
import Types.ResourceManager
import Skills.Runes (runeSetByLevel)

import Entity.Utils
import Entity.Actions

initScript :: ScriptName -> Script
initScript = \case
    ScriptName_Bertram -> Script_Bertram def
    ScriptName_Nop     -> Script_Nop

-- TODO: auto-reload scripts on hs file change with hint lib...
setupScript :: Update Agent ()
setupScript = use (self.script) >>= \case
    Script_Nop -> whenJustM (use $ self.agentType.scriptName) $ \n ->
        self.script .= initScript n
    _ -> initWhenScriptChanged
    where
    initWhenScriptChanged = return ()

updateScript :: DialogAction -> Update Agent ()
updateScript a = use (self.script) >>= \case
    Script_Bertram s -> assign (self.script) . Script_Bertram =<< updateBertram a s
    Script_Nop       -> return ()

stepScript :: Update Agent ()
stepScript = use (self.script) >>= \case
    Script_Bertram s -> assign (self.script) . Script_Bertram =<< stepBertram s
    Script_Nop       -> return ()

--------------------------------------------------------------------------------

updateBertram
    :: DialogAction -> ScriptStateBertram -> Update Agent ScriptStateBertram
updateBertram a s = case a of
    DialogAction_Start    -> startConversation
    DialogAction_NextPage -> progressConversation
    where
    startConversation = do
        startStoryDialog StoryDialogName_BertramWaiting
        return s

    progressConversation = case s^.ff#storyStatus of
        StoryStatus_Wait -> return s
        StoryStatus_InitialRunes -> do
            whenJustM queryPlayer $ \p -> do
                -- rs <- use $ context.resources.runeSet
                -- addRunes p $ runeSetByLevel 1 rs
                return ()
            nextStatus
        _ -> nextStatus

    nextStatus = return $ s & ff#storyStatus %~ nextStop

stepBertram :: ScriptStateBertram -> Update Agent ScriptStateBertram
stepBertram s = do
    case s^.ff#storyStatus of
        StoryStatus_Start -> startWelcomeSequence
        _ -> return s
    where
    startWelcomeSequence = do
        startStoryDialog StoryDialogName_BertramWelcome
        return $ s & ff#storyStatus .~ StoryStatus_Welcome

startStoryDialog :: StoryDialogName -> Update Agent ()
startStoryDialog n = do
    sid <- useSelfId
    whenJustM (getStoryDialog n) $ addWorldAction . WorldAction_StoryDialog sid
    where
    getStoryDialog = uses (context.resources.dialogMap) . HashMap.lookup

