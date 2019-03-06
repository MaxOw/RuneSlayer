module Skills.Runes
    ( RunicLevel, RuneUsage

    , updateUsage
    ) where

import Delude
import qualified Data.HashMap.Strict as HashMap

import Types.Skills.Runes

--------------------------------------------------------------------------------

updateUsage :: RuneName -> RuneUsage -> RunicLevel -> RunicLevel
updateUsage n u l = l
    & mastery %~ HashMap.alter alterMastery n
    where
    alterMastery = Just . updateMastery . fromMaybe def
    updateMastery m = m
        & usageHistory %~ (u:)
        & masteryLevel %~ updateLevel
        & if u^.success then successCount +~ 1 else failureCount +~ 1
    updateLevel
        | u^.success = min 50 . (\x->x+1)
        | otherwise  = max  0 . (\x->x-5)
