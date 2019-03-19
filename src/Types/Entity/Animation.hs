{-# Language TemplateHaskell #-}
module Types.Entity.Animation where

import Delude
import Engine (RenderAction)
import Types.Entity.Common
import Types.Sprite

--------------------------------------------------------------------------------

data AnimationDirection
   = North
   | West
   | South
   | East
   deriving (Generic, Eq, Ord, Enum, Bounded)

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
   | CharacterAnimation FilePath
   deriving (Generic)

data AnimationFrame = AnimationFrame
   { animationFrame_duration :: Float
   , animationFrame_sprite   :: SpriteDesc
   } deriving (Generic)

data AnimationPart = AnimationPart
   { animationPart_direction :: Maybe AnimationDirection
   , animationPart_kind      :: Maybe AnimationKind
   , animationPart_frames    :: [AnimationFrame]
   } deriving (Generic)

--------------------------------------------------------------------------------

data Animation = Animation
   { animation_aniMap      :: AnimationState -> Maybe SpriteDesc
   , animation_current     :: AnimationState
   , animation_progression :: AnimationProgression
   , animation_speed       :: Float
   }

data AnimationState = AnimationState
   { animationState_direction :: AnimationDirection
   , animationState_kind      :: AnimationKind
   , animationState_era       :: Float
   }

data AnimationProgression
   = Stopped
   | Cycle
   | TransitionInto AnimationKind AnimationProgression
   deriving (Eq)

data EffectKind
   = HitEffect AttackPower

data EffectState = EffectState
   { effectState_effectUpdate :: Time -> EffectState -> Maybe EffectState
   , effectState_kind         :: EffectKind
   , effectState_duration     :: Float
   , effectState_era          :: Float
   }

--------------------------------------------------------------------------------

instance ToJSON AnimationDirection where
    toEncoding = genericToEncoding customOptionsJSON
instance ToJSON AnimationKind  where toEncoding = genericToEncoding customOptionsJSON
instance ToJSON AnimationDesc  where toEncoding = genericToEncoding customOptionsJSON
instance ToJSON AnimationFrame where toEncoding = genericToEncoding customOptionsJSON
instance ToJSON AnimationPart  where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON AnimationDirection where
    parseJSON = genericParseJSON customOptionsJSON
instance FromJSON AnimationKind  where parseJSON = genericParseJSON customOptionsJSON
instance FromJSON AnimationDesc  where parseJSON = genericParseJSON customOptionsJSON
instance FromJSON AnimationFrame where parseJSON = genericParseJSON customOptionsJSON
instance FromJSON AnimationPart  where parseJSON = genericParseJSON customOptionsJSON

instance Default AnimationDesc where
    def = CustomAnimation []

instance Default Animation where
    def = Animation
        { animation_aniMap      = \_ -> Nothing
        , animation_current     = def
        , animation_progression = Stopped
        , animation_speed       = 1
        }

instance Default AnimationState where
    def = AnimationState
        { animationState_direction = South
        , animationState_kind      = Walk
        , animationState_era       = 0
        }

makeFieldsCustom ''AnimationFrame
makeFieldsCustom ''AnimationPart

makeFieldsCustom ''Animation
makeFieldsCustom ''AnimationState
makeFieldsCustom ''EffectState
