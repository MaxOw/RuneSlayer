module Types.Entity.Script where

import Delude
import Dhall (Interpret)

data ScriptName
   = ScriptName_Bertram
   | ScriptName_Nop
   deriving (Generic)

data ScriptStateBertram = ScriptStateBertram
   { field_welcomeDone :: Bool
   } deriving (Generic)
instance Default ScriptStateBertram

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
