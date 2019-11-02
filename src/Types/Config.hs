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

data FontDesc = FontDesc
   { field_name       :: Text
   , field_regular    :: FilePath
   , field_bold       :: Maybe FilePath
   , field_italic     :: Maybe FilePath
   , field_boldItalic :: Maybe FilePath
   } deriving (Generic)

data FontsConfig = FontsConfig
   { field_fontsPath   :: Maybe FilePath
   , field_fonts       :: [FontDesc]
   , field_defaultFont :: Text
   } deriving (Generic)

