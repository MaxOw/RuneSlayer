module Types.GUI (module Types.GUI) where

import Delude

import Types.GUI.Common as Types.GUI
import Types.EntityAction (AttackMode)

--------------------------------------------------------------------------------

data Status = Status
   { field_hostilesInRange :: Bool
   , field_itemsInRange    :: Bool
   , field_attackMode      :: AttackMode
   , field_health          :: StatusPoints
   , field_runes           :: StatusPoints
   } deriving (Generic)
instance Default Status

--------------------------------------------------------------------------------

data StatusPoints = StatusPoints
   { field_points    :: Int
   , field_maxPoints :: Int
   } deriving (Generic)
instance Default StatusPoints

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

data RunicMode = RunicMode
   { field_showQuery  :: Bool
   , field_queryText  :: Text
   , field_answerText :: Text
   } deriving (Generic)
instance Default RunicMode

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

