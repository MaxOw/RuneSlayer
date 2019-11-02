module Types.Config where

import Delude
import Types.InputKeymap (KeymapEntry)

--------------------------------------------------------------------------------

data DebugMode
   = DebugMode_WorldGen
   | DebugMode_Nothing
   deriving (Generic)

data ConfigDebugFlag
   = ConfigDebugFlag_NoStory
   | ConfigDebugFlag_NoTutorial
   deriving (Generic, Enum)

data Config = Config
   { field_debugFlags           :: BitSet32 ConfigDebugFlag
   , field_runeSet              :: Text
   , field_clearDefaultBindings :: Bool
   , field_bindings             :: [KeymapEntry Text]
   , field_dhallPath            :: Maybe Text
   } deriving (Generic)
instance Default Config

