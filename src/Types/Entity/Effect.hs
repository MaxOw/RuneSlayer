module Types.Entity.Effect where

import Delude
import Types.Entity.Common
import Types.Entity.ZIndex

--------------------------------------------------------------------------------

data EffectKind
   = HitEffect AttackPower

data Effect = Effect
   { field_location     :: Location
   , field_kind         :: EffectKind
   , field_duration     :: Duration
   , field_era          :: Float
   } deriving (Generic)

instance GetZIndex Effect Word32 where
    get_zindex _ = toZIndex EntityZIndex_Vertical

instance Default Effect where
    def = Effect
        { field_location = def
        , field_kind     = HitEffect 0
        , field_duration = 1
        , field_era      = def
        }
