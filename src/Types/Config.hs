module Types.Config where

import Delude
import Dhall (Interpret)

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
   { field_debugFlags :: BitSet32 ConfigDebugFlag
   , field_runeSet    :: Text
   } deriving (Generic)

--------------------------------------------------------------------------------

instance Default Config
instance Interpret ConfigDebugFlag
instance Interpret DebugMode
instance Interpret Config
