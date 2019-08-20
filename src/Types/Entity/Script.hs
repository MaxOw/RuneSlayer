module Types.Entity.Script where

import Delude
import Dhall (Interpret)

data ScriptName
   = ScriptName_Bertram
   | ScriptName_Nop
   deriving (Generic)

data StoryStatus
   = StoryStatus_Start
   | StoryStatus_Welcome
   | StoryStatus_InitialRunes
   | StoryStatus_Wait
   deriving (Generic, Enum, Eq, Bounded)

data ScriptStateBertram = ScriptStateBertram
   { field_storyStatus :: StoryStatus
   } deriving (Generic)
instance Default ScriptStateBertram where
    def = ScriptStateBertram
        { field_storyStatus = StoryStatus_Start
        }

data Script
   = Script_Bertram ScriptStateBertram
   | Script_Nop
   -- deriving (Generic)
instance Default Script where def = Script_Nop

data StoryDialogName
   = StoryDialogName_BertramWelcome
   | StoryDialogName_BertramWaiting
   deriving (Eq, Ord, Show, Generic)
instance Hashable StoryDialogName

data StoryDialog = StoryDialog
   { field_name        :: StoryDialogName
   , field_title       :: Text
   , field_dialogPages :: [Text]
   } deriving (Show, Generic)

--------------------------------------------------------------------------------

instance Interpret StoryDialogName
instance Interpret StoryDialog

instance ToJSON   ScriptName where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON ScriptName where parseJSON  = genericParseJSON  customOptionsJSON

--------------------------------------------------------------------------------

{-
data ScriptSum
   = ScriptSum_Bertram ScriptStateBertram
   | ScriptSum_Nop

class ScriptLike s where
    initScript   :: ScriptSum -> s
    updateScript :: DialogAction -> s -> Update Agent s
    stepScript   :: s -> Update Agent s
    saveScript   :: s -> ScriptSum

newtype Script = Script (forall s. ScriptLike s => s)
instance ScriptLike Script where
    initScript s = Script $ case s of
        ScriptSum_Bertram x -> initScript x
        ScriptSum_Nop       -> initScript ()
    updateScript a (Script s) = Script <$> updateScript a s
    stepScript (Script s) = Script <$> stepScript s
    saveScript (Script s) = saveScript s
-}

