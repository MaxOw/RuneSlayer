module Entity.Script where

import Delude
import qualified Data.HashMap.Strict as HashMap

import Types.Entity.Agent
import Types.Entity.Script
import Types.ResourceManager

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

stepScript :: Update Agent ()
stepScript = use (self.script) >>= \case
    Script_Bertram s -> assign (self.script) . Script_Bertram =<< stepBertram s
    Script_Nop       -> return ()

stepBertram :: ScriptStateBertram -> Update Agent ScriptStateBertram
stepBertram s = do
    unless (s^.ff#welcomeDone) startWelcomeSequence
    return $ s
        & ff#welcomeDone .~ True
    where
    startWelcomeSequence = do
        whenJustM (getStoryDialog StoryDialogName_BertramWelcome) $
            addWorldAction . WorldAction_StoryDialog
        return ()

    getStoryDialog = uses (context.resources.dialogMap) . HashMap.lookup

