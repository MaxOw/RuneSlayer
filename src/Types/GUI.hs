module Types.GUI (module Types.GUI) where

import Delude

import Types.GUI.Common as Types.GUI
import Types.EntityAction (AttackMode)

--------------------------------------------------------------------------------

data StatusDesc = StatusDesc
   { field_hostilesInRange :: Bool
   , field_attackMode      :: AttackMode
   } deriving (Generic)
instance Default StatusDesc

--------------------------------------------------------------------------------

data SlotDesc = SlotDesc
   { field_percent :: Float
   } deriving (Generic)

data SlotsPanelDesc = SlotsPanelDesc
   { field_slots      :: [SlotDesc]
   , field_showQuery  :: Bool
   , field_queryText  :: Text
   , field_answerText :: Text
   } deriving (Generic)
instance Default SlotsPanelDesc
