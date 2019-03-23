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
   | CharacterAnimation SpriteDesc
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

newtype Animation = Animation
    { runAnimation :: AnimationFrameState -> RenderAction }
    deriving (Semigroup, Monoid)

data AnimationState = AnimationState
   { animationState_current     :: AnimationFrameState
   , animationState_progression :: AnimationProgression
   , animationState_speed       :: Float
   }

data AnimationFrameState = AnimationFrameState
   { animationFrameState_direction :: AnimationDirection
   , animationFrameState_kind      :: AnimationKind
   , animationFrameState_era       :: Float
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

instance Default AnimationState where
    def = AnimationState
        { animationState_current     = def
        , animationState_progression = Stopped
        , animationState_speed       = 1
        }

instance Default AnimationFrameState where
    def = AnimationFrameState
        { animationFrameState_direction = South
        , animationFrameState_kind      = Walk
        , animationFrameState_era       = 0
        }

instance Default Animation where
    def = Animation $ const mempty

makeFieldsCustom ''AnimationFrame
makeFieldsCustom ''AnimationPart

makeFieldsCustom ''AnimationState
makeFieldsCustom ''AnimationFrameState
makeFieldsCustom ''EffectState
