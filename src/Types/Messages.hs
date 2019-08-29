module Types.Messages where

import Delude
import Types.Entity.Common (Duration, timeInSeconds)

data SystemMessage = SystemMessage
   { field_content  :: Text
   , field_duration :: Duration
   } deriving (Generic)

data SystemMessages = SystemMessages
   { field_maxMessagesCount   :: Int
   , field_maxDisplayDuration :: Duration
   , field_messages           :: [SystemMessage]
   } deriving (Generic)
instance Default SystemMessages where
    def = SystemMessages
        { field_maxMessagesCount   = 8
        , field_maxDisplayDuration = timeInSeconds 5
        , field_messages           = def
        }

messages :: Lens' SystemMessages [SystemMessage]
messages = ff#messages
