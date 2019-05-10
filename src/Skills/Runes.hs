module Skills.Runes
    ( RunicLevel, RuneUsage

    -- , updateUsage
    , initRunicSlots
    , fillRunicSlot
    , dischargeRunicSlot
    , listRunicSlots
    ) where

import Delude
-- import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap

import Types.Skills.Runes

--------------------------------------------------------------------------------

{-
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
-}

initRunicSlots :: Int -> RunicSlots
initRunicSlots n = def
    & ff#slotsCount .~ n
    & slots         .~ IntMap.fromList (map (,0) [0..n-1])

fillRunicSlot :: RunicSlots -> RunicSlots
fillRunicSlot x = case mk of
    Nothing -> x
    Just sk -> x & slots %~ IntMap.insert sk 1
    where
    mk = viaNonEmpty head $ map fst $ sortOn snd
       $ IntMap.toList $ x^.slots

listRunicSlots :: RunicSlots -> [Float]
listRunicSlots r
    = map (fromMaybe 0 . flip IntMap.lookup m) $ take n [0..]
    where
    m = r^.ff#slots
    n = r^.ff#slotsCount

dischargeRunicSlot :: RunicSlots -> RunicSlots
dischargeRunicSlot x = case mk of
    Nothing -> x
    Just sk -> x & slots %~ IntMap.insert sk 0
    where
    mk = viaNonEmpty head $ map fst $ filter ((>0) . snd) $ sortOn snd
       $ IntMap.toList $ x^.slots

