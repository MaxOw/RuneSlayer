{-# Language TemplateHaskell #-}
module Types.Skills.Runes where

import Delude
-- import Data.Time.Clock (UTCTime)

--------------------------------------------------------------------------------

newtype RunePowerLevel = RunePowerLevel Int deriving (Num)
newtype RuneName       = RuneName Text deriving (Eq, Hashable)

data Rune = Rune
   { rune_name    :: RuneName
   , rune_reading :: Text -- TODO: allow multiple
   , rune_level   :: RunePowerLevel
   }

data RuneMastery = RuneMastery
   { runeMastery_successCount  :: Int
   , runeMastery_failureCount  :: Int
   , runeMastery_masteryLevel  :: Int
   , runeMastery_usageHistory  :: [RuneUsage]
   } deriving (Generic)
instance Default RuneMastery

data RuneUsage = RuneUsage
   -- { runeUsage_timestamp :: UTCTime
   { runeUsage_success   :: Bool
   }

data RunicLevel = RunicLevel
   { runicLevel_knwon   :: HashMap RuneName Rune
   , runicLevel_mastery :: HashMap RuneName RuneMastery
   , runicLevel_level   :: RunePowerLevel -- cumulative power
   }

instance Default RunicLevel where
    def = RunicLevel
        { runicLevel_knwon   = mempty
        , runicLevel_mastery = mempty
        , runicLevel_level   = 0
        }

makeFieldsCustom ''Rune
makeFieldsCustom ''RuneMastery
makeFieldsCustom ''RuneUsage
makeFieldsCustom ''RunicLevel
