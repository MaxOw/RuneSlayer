module Types.GUI (module Types.GUI) where

import Delude

import Types.GUI.Common as Types.GUI
import Types.Entity.Common (Health)
import Types.EntityAction (AttackMode)

--------------------------------------------------------------------------------

data Status = Status
   { field_hostilesInRange :: Bool
   , field_attackMode      :: AttackMode
   } deriving (Generic)
instance Default Status

--------------------------------------------------------------------------------

data HealthStatus = HealthStatus
   { field_health    :: Health
   , field_maxHealth :: Health
   } deriving (Generic)
instance Default HealthStatus

--------------------------------------------------------------------------------

data ActionHint = ActionHint
   { field_actionName :: Text
   , field_actionHint :: Text
   } deriving (Generic)
instance Default ActionHint

--------------------------------------------------------------------------------

data StoryDialog = StoryDialog
   { field_title       :: Text
   , field_content     :: Text
   , field_nextPage    :: Bool
   , field_nextPageKey :: Text
   } deriving (Generic)
instance Default StoryDialog

--------------------------------------------------------------------------------

data Slot = Slot
   { field_percent :: Float
   } deriving (Generic)

data SlotsPanel = SlotsPanel
   { field_slots      :: [Slot]
   , field_showQuery  :: Bool
   , field_queryText  :: Text
   , field_answerText :: Text
   } deriving (Generic)
instance Default SlotsPanel

--------------------------------------------------------------------------------

data SelectEntry = SelectEntry
   { field_prefix    :: Maybe String
   , field_label     :: Maybe Text
   , field_hint      :: Maybe String
   , field_isFocused :: Bool
   , field_content   :: Maybe Text
   } deriving (Generic)
instance Default SelectEntry

data Description = Description
   { field_name :: Text
   } deriving (Generic)
instance Default Description

data Container = Container
   { field_title   :: Text
   , field_hint    :: Text
   , field_content :: [SelectEntry]
   } deriving (Generic)
instance Default Container

data Inventory = Inventory
   { field_equipment   :: [SelectEntry]
   , field_description :: Maybe Description
   , field_containers  :: [Container]
   } deriving (Generic)
instance Default Inventory

