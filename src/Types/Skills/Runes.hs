{-# Language DeriveDataTypeable #-}
{-# Language TemplateHaskell #-}
module Types.Skills.Runes where

import Delude hiding (Indexable)
import Data.Time.Clock (UTCTime)

import Data.Data (Data)
import Data.IxSet.Typed

--------------------------------------------------------------------------------

newtype RunicPoints = RunicPoints Int
    deriving (Eq, Ord, Data, Show, FromJSON, Num, Default, Generic)
instance Wrapped RunicPoints
instance Rewrapped RunicPoints RunicPoints

--------------------------------------------------------------------------------

newtype RunePowerLevel = RunePowerLevel Int
    deriving (Eq, Ord, Data, Show, FromJSON, Num, Default, Generic)
instance Wrapped RunePowerLevel
instance Rewrapped RunePowerLevel RunePowerLevel

newtype RuneName = RuneName Text
    deriving (Eq, Ord, Data, Show, FromJSON, Hashable, Default)
newtype RuneKind = RuneKind Text
    deriving (Eq, Ord, Data, Show, FromJSON, Default)

data ValidationStrategy
   = ValidationStrategy_Exact
   | ValidationStrategy_Levenshtein Int
   deriving (Show, Generic, Eq, Ord, Data)
instance FromJSON ValidationStrategy where
    parseJSON = genericParseJSON customOptionsJSON

data Rune = Rune
   { field_name       :: RuneName
   , field_runeId     :: Text
   , field_kind       :: RuneKind
   , field_level      :: RunePowerLevel
   , field_query      :: Text
   , field_answers    :: [Text]
   -- , field_validation :: ValidationStrategy
   } deriving (Eq, Ord, Data, Generic, Show)

type RuneIxs = '[RuneName, RuneKind, RunePowerLevel]
type RuneSet = IxSet RuneIxs Rune

instance Indexable RuneIxs Rune where
    indices = ixList
        (ixGen (Proxy @RuneName))
        (ixGen (Proxy @RuneKind))
        (ixGen (Proxy @RunePowerLevel))

instance FromJSON Rune where parseJSON = genericParseJSON customOptionsJSON

newtype MasteryLevel = MasteryLevel Int deriving (Default, Data, Eq, Ord, Num)
newtype SuccessCount = SuccessCount Int deriving (Default, Data, Eq, Ord, Num)
newtype FailureCount = FailureCount Int deriving (Default, Data, Eq, Ord, Num)
newtype     UseCount =     UseCount Int deriving (Default, Data, Eq, Ord, Num)

data RuneMastery = RuneMastery
   { field_name          :: RuneName
   , field_runeId        :: Text
   , field_kind          :: RuneKind
   , field_level         :: RunePowerLevel
   , field_query         :: Text
   , field_answers       :: [Text]
   , field_successCount  :: Int
   , field_failureCount  :: Int
   , field_masteryLevel  :: MasteryLevel
   , field_usageHistory  :: [RuneUsage]
   } deriving (Generic, Data, Eq, Ord)
instance Default RuneMastery

type MasteryIxs =
   '[ RuneName, RuneKind, RunePowerLevel
    , SuccessCount, FailureCount, UseCount, MasteryLevel]
type MasterySet = IxSet MasteryIxs RuneMastery

instance Indexable MasteryIxs RuneMastery where
    indices = ixList
        (ixGen (Proxy @RuneName))
        (ixGen (Proxy @RuneKind))
        (ixGen (Proxy @RunePowerLevel))
        (ixFun $ \m -> [SuccessCount $ m^.ff#successCount])
        (ixFun $ \m -> [FailureCount $ m^.ff#failureCount])
        (ixFun $ \m -> [    UseCount $ m^.ff#successCount + m^.ff#failureCount])
        (ixGen (Proxy @MasteryLevel))

data RuneUsage = RuneUsage
   { field_timestamp :: UTCTime
   , field_success   :: Bool
   } deriving (Generic, Data, Eq, Ord)

data RunicLevel = RunicLevel
   { field_mastery  :: MasterySet
   , field_lastUsed :: Maybe RuneName
   , field_level    :: RunePowerLevel -- cumulative power
   , field_count    :: Int -- Counter of runes asked
   } deriving (Generic)

instance Default RunicLevel where
    def = RunicLevel
        { field_mastery  = mempty
        , field_lastUsed = def
        , field_level    = 2
        , field_count    = 0
        }

