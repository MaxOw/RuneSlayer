module Types.Runes where

import Delude
import Types.Skills.Runes

--------------------------------------------------------------------------------

data RuneResult = RuneResult
   { field_rune   :: Rune
   , field_answer :: Text
   , field_result :: Bool
   } deriving (Generic)

data RunesState = RunesState
   { field_allRunes       :: RuneSet
   , field_mastery        :: MasterySet
   , field_currentRune    :: Maybe Rune
   , field_lastRuneName   :: Maybe RuneName
   , field_lastRuneResult :: Maybe RuneResult
   , field_level          :: RunePowerLevel
   } deriving (Generic)

mastery :: Lens' RunesState MasterySet
mastery = ff#mastery

lastRuneName :: Lens' RunesState (Maybe RuneName)
lastRuneName = ff#lastRuneName

lastRuneResult :: Lens' RunesState (Maybe RuneResult)
lastRuneResult = ff#lastRuneResult

--------------------------------------------------------------------------------
