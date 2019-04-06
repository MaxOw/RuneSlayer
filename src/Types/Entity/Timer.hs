module Types.Entity.Timer where

import Delude
import Types.Entity.Common (Duration)
import Data.DefMap (DefMap)

--------------------------------------------------------------------------------

data TimerType
   = Timer_Attack
   | Timer_Bla
   deriving (Eq, Ord, Generic)

newtype Timer = Timer (DefMap TimerType Duration)
    deriving (Generic, Default)

--------------------------------------------------------------------------------

instance ToJSON   TimerType where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON TimerType where parseJSON  = genericParseJSON  customOptionsJSON

