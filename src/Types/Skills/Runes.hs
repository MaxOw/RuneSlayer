module Types.Skills.Runes where

import Delude
-- import Data.Time.Clock (UTCTime)


--------------------------------------------------------------------------------

data RunicSlots = RunicSlots
   { field_slotsCount :: Int
   , field_slots      :: IntMap Float
   } deriving (Generic)
instance Default RunicSlots

--------------------------------------------------------------------------------

newtype RunePowerLevel = RunePowerLevel Int deriving (Num)
newtype RuneName       = RuneName Text deriving (Eq, Hashable)

data Rune = Rune
   { field_name    :: RuneName
   , field_reading :: Text -- TODO: allow multiple
   , field_level   :: RunePowerLevel
   }

data RuneMastery = RuneMastery
   { field_successCount  :: Int
   , field_failureCount  :: Int
   , field_masteryLevel  :: Int
   , field_usageHistory  :: [RuneUsage]
   } deriving (Generic)
instance Default RuneMastery

data RuneUsage = RuneUsage
   -- { field_timestamp :: UTCTime
   { field_success   :: Bool
   }

data RunicLevel = RunicLevel
   { field_knwon   :: HashMap RuneName Rune
   , field_mastery :: HashMap RuneName RuneMastery
   , field_level   :: RunePowerLevel -- cumulative power
   }

instance Default RunicLevel where
    def = RunicLevel
        { field_knwon   = mempty
        , field_mastery = mempty
        , field_level   = 0
        }





