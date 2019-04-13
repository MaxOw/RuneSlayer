module Types.Config where

import Delude

--------------------------------------------------------------------------------

data DebugMode
   = DebugMode_WorldGen
   | DebugMode_Nothing
   deriving (Generic)

data Config = Config
   { field_debugMode :: Maybe DebugMode
   } deriving (Generic)

--------------------------------------------------------------------------------

instance Default Config

instance FromJSON DebugMode where parseJSON  = genericParseJSON  customOptionsJSON
instance FromJSON Config    where parseJSON  = genericParseJSON  customOptionsJSON
