module Skills.Runes
    ( RuneSet, RunicLevel, RunicSlots, RuneUsage(..)

    , updateUsage
    , selectRune
    , getRuneByName
    , addKnownRunes
    , isCorrectAnswer

    , initRunicSlots
    , fillRunicSlot
    , dischargeRunicSlot
    , listRunicSlots

    , buildRuneSet
    -- , getRunesByKind
    , runeSetByLevel
    , runeSetByName
    -- , getRunesByKindAndLevel
    {-
    , getNextRune
    -}
    ) where

import Delude hiding (Indexable)
import Data.Time.Clock (UTCTime)
import Data.Generics.Product.Subtype (smash)
import qualified Data.IntMap as IntMap

import Types.Skills.Runes

import qualified Data.IxSet.Typed as IxSet
import Data.IxSet.Typed (IxSet, Indexable, IsIndexOf)

--------------------------------------------------------------------------------

maxMasteryLevel :: MasteryLevel
maxMasteryLevel = 10

updateUsage :: RuneName -> RuneUsage -> RunicLevel -> RunicLevel
updateUsage n u l = l
    & ff#mastery  %~ alterIx alterMastery n
    & ff#lastUsed .~ Just n
    & ff#count    +~ 1
    where
    alterMastery = Just . updateMastery . fromMaybe defRune
    updateMastery m = m
        & ff#usageHistory %~ (u:)
        & ff#masteryLevel %~ updateLevel
        & if u^.ff#success then ff#successCount +~ 1 else ff#failureCount +~ 1
    defRune = def & name .~ n
    updateLevel
        | u^.ff#success = min maxMasteryLevel . (\x->x+1)
        | otherwise     = max               0 . (\x->x-2)

alterIx
    :: Indexable ixs a
    => IsIndexOf ix ixs
    => (Maybe a -> Maybe a) -> ix -> IxSet ixs a -> IxSet ixs a
alterIx f k s = case IxSet.toList $ IxSet.getEQ k s of
    []  -> maybeInsert $ f Nothing
    [v] -> maybeInsert $ f (Just v)
    _   -> s
    where
    maybeInsert = \case
        Nothing -> IxSet.deleteIx k s
        Just nv -> IxSet.insert nv $ IxSet.deleteIx k s

{-
getMasteryRatio :: MasteryLevel -> Float
getMasteryRatio m = conv m / conv maxMasteryLevel
    where
    conv (MasteryLevel x) = fromIntegral x
-}

selectRune :: UTCTime -> RunicLevel -> Maybe RuneName
selectRune _curtime rl = getFirstProper runesList
    where
    getFirstProper = \case
        []      -> Nothing
        (a:[])  -> Just a
        (a:b:_) -> Just $ if Just a == rl^.ff#lastUsed then b else a

    runesList = map (view name) levRunesList
    {-
    priorityList = base <> allRunes
    base = concatMap IxSet.toList [newRunes, priRunes]
    newRunes = IxSet.getLTE (UseCount 0) (rl^.ff#mastery)
    priRunes = IxSet.getLTE (UseCount 3) (rl^.ff#mastery)
    allRunes = IxSet.toList (rl^.ff#mastery)
    -}

    levRunesList = IxSet.toAscList (Proxy @MasteryLevel) (rl^.ff#mastery)
    -- oldRunesList = [] -- sort runes from oldest

getRuneByName :: RuneName -> RunicLevel -> Maybe RuneMastery
getRuneByName n rl = IxSet.getOne $ IxSet.getEQ n (rl^.ff#mastery)

addKnownRunes :: RuneSet -> RunicLevel -> RunicLevel
addKnownRunes rs = over (ff#mastery) ins
    where
    ins = IxSet.insertList . map uprune $ toList rs
    uprune :: Rune -> RuneMastery
    uprune x = smash x def

isCorrectAnswer :: HasF "answers" r [Text] => Text -> r -> Bool
isCorrectAnswer ans = any (==ans) . view (ff#answers)

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

buildRuneSet :: [Rune] -> RuneSet
buildRuneSet = IxSet.fromList

-- getRunesByKind :: RuneKind -> RuneSet -> [Rune]
-- getRunesByKind v s = IxSet.toList $ IxSet.getEQ v s

runeSetByLevel :: RunePowerLevel -> RuneSet -> RuneSet
runeSetByLevel = IxSet.getEQ

runeSetByName :: RuneName -> RuneSet -> RuneSet
runeSetByName = IxSet.getEQ

-- getRunesByKindAndLevel :: RuneKind -> RunePowerLevel -> RuneSet -> [Rune]
-- getRunesByKindAndLevel k l s = IxSet.toList $ IxSet.getEQ l $ IxSet.getEQ k s

{-
getNextRune :: RuneName -> RuneSet -> Maybe Rune
getNextRune = undefined
-}

