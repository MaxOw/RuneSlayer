module Types.Entity.Animation where

import Delude
import Data.Aeson (ToJSONKey, FromJSONKey)
import Engine (RenderAction)
import Types.Entity.Common
import Types.Sprite

--------------------------------------------------------------------------------

data Direction
   = North
   | West
   | South
   | East
   deriving (Generic, Show, Eq, Ord, Enum, Bounded)

data AnimationKind
   = Cast
   | Thrust
   | Walk
   | Slash
   | Fire
   | Die
   deriving (Generic, Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------

data AnimationDesc
   = CustomAnimation    [AnimationPart]
   | CharacterAnimation SpriteDesc
   deriving (Generic)

data AnimationFrame = AnimationFrame
   { field_duration :: Duration
   , field_sprite   :: SpriteDesc
   } deriving (Generic)

data AnimationPart = AnimationPart
   { field_direction :: Maybe Direction
   , field_kind      :: Maybe AnimationKind
   , field_frames    :: [AnimationFrame]
   } deriving (Generic)

--------------------------------------------------------------------------------

newtype AnimationName = AnimationName Text
   deriving (Eq, Hashable, Generic, FromJSON, ToJSON, ToJSONKey, FromJSONKey)

newtype Animation = Animation
    { runAnimation :: AnimationFrameState -> RenderAction }
    deriving (Semigroup, Monoid)

data AnimationState = AnimationState
   { field_current     :: AnimationFrameState
   , field_progression :: AnimationProgression
   , field_speed       :: Float
   } deriving (Generic)

data AnimationFrameState = AnimationFrameState
   { field_direction :: Direction
   , field_kind      :: AnimationKind
   , field_era       :: Float
   } deriving (Generic)

data AnimationProgression
   = Stopped
   | Cycle
   | TransitionInto AnimationKind AnimationProgression
   deriving (Eq)

--------------------------------------------------------------------------------

instance ToJSON Direction where
    toEncoding = genericToEncoding customOptionsJSON
instance ToJSON AnimationKind  where toEncoding = genericToEncoding customOptionsJSON
instance ToJSON AnimationDesc  where toEncoding = genericToEncoding customOptionsJSON
instance ToJSON AnimationFrame where toEncoding = genericToEncoding customOptionsJSON
instance ToJSON AnimationPart  where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON Direction where
    parseJSON = genericParseJSON customOptionsJSON
instance FromJSON AnimationKind  where parseJSON = genericParseJSON customOptionsJSON
instance FromJSON AnimationDesc  where parseJSON = genericParseJSON customOptionsJSON
instance FromJSON AnimationFrame where parseJSON = genericParseJSON customOptionsJSON
instance FromJSON AnimationPart  where parseJSON = genericParseJSON customOptionsJSON

instance Default AnimationDesc where
    def = CustomAnimation []

instance Default AnimationState where
    def = AnimationState
        { field_current     = def
        , field_progression = Stopped
        , field_speed       = 1
        }

instance Default AnimationFrameState where
    def = AnimationFrameState
        { field_direction = South
        , field_kind      = Walk
        , field_era       = 0
        }

instance Default Animation where
    def = Animation $ const mempty
