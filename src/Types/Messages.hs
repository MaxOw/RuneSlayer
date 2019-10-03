module Types.Messages where

import Delude
import Types.Entity.Common (Duration, Location, timeInSeconds)

data MessageKind
   = MessageKind_Info
   | MessageKind_HitEffect
   deriving (Eq)

data SystemMessage = SystemMessage
   { field_content     :: Text
   , field_location    :: Maybe Location
   , field_messageKind :: MessageKind
   , field_duration    :: Duration
   , field_maxDuration :: Duration
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
