module Types.Story where

import Delude
import Types.Entity (EntityId)
import Types.Entity.Agent (ScriptName)
import Data.Zipper (Zipper)
import Data.Timer (Timer)
import Dhall (Interpret)

data StoryStep
   = StoryStep_Start
   | StoryStep_Welcome
   | StoryStep_Done
   deriving (Eq, Ord, Enum, Bounded)

data TimerType
   = TimerType_Delay
   deriving (Eq, Ord)

data StoryDialog = StoryDialog
    { field_title       :: Text
    , field_entityId    :: EntityId
    , field_scriptName  :: ScriptName
    , field_dialogPages :: Zipper Text
    } deriving (Generic)
instance HasEntityId StoryDialog EntityId

data BertramState
   = BertramState_Welcome
   | BertramState_Waiting
   deriving (Eq, Ord, Enum, Bounded)

data BertramDialogMap = BertramDialogMap
   { field_welcome :: [Text]
   , field_waiting :: [Text]
   } deriving (Generic)
instance Interpret BertramDialogMap

data DialogMap = DialogMap
   { field_bertram :: BertramDialogMap
   } deriving (Generic)
instance Interpret DialogMap

data StoryState = StoryState
   { field_currentStep  :: StoryStep
   , field_started      :: Set StoryStep
   , field_storyDialog  :: Maybe StoryDialog
   , field_register     :: Map ScriptName EntityId
   , field_bertramState :: BertramState
   , field_dialogMap    :: DialogMap
   , field_timer     :: Timer TimerType
   } deriving (Generic)

currentStep :: Lens' StoryState StoryStep
currentStep = ff#currentStep

storyDialog :: Lens' StoryState (Maybe StoryDialog)
storyDialog = ff#storyDialog

bertramState :: Lens' StoryState BertramState
bertramState = ff#bertramState

register :: Lens' StoryState (Map ScriptName EntityId)
register = ff#register

