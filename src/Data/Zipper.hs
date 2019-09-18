module Data.Zipper
    ( Zipper
    , fromList, empty
    , null, isLeftmost, isRightmost
    , focus
    , left, right
    , leftmost, rightmost
    , leftCycle, rightCycle
    ) where

import Relude hiding (fromList, empty, null)
import Data.Default

-- | List zipper data type.
data Zipper a = Zipper [a] [a]
instance Default (Zipper a) where def = empty

-- | Create zipper from list.
fromList :: [a] -> Zipper a
fromList ls = Zipper ls []

-- | Create empty zipper.
empty :: Zipper a
empty = Zipper [] []

-- | Check if zipper is empty.
null :: Zipper a -> Bool
null = \case
    Zipper [] [] -> True
    _            -> False

-- | Check if zipper is at the leftmost position.
isLeftmost :: Zipper a -> Bool
isLeftmost = \case
    Zipper _ [] -> True
    _           -> False

-- | Check if zipper is at the rightmost position.
isRightmost :: Zipper a -> Bool
isRightmost = \case
    Zipper (_:_:_)  _ -> False
    _                 -> True

-- | Get element currently in zippers focus, or Nothing when empty.
focus :: Zipper a -> Maybe a
focus (Zipper ls _) = viaNonEmpty head ls

-- | Move zipper one tooth to the left if possible.
left :: Zipper a -> Zipper a
left z@(Zipper ls rs) = case rs of
    (a:as) -> Zipper (a:ls) as
    _      -> z

-- | Move zipper one tooth to the right if possible.
right :: Zipper a -> Zipper a
right z@(Zipper ls rs) = case ls of
    []     -> z
    [_]    -> z
    (a:as) -> Zipper as (a:rs)

-- | Move zipper to the leftmost position.
leftmost :: Zipper a -> Zipper a
leftmost z
    | isLeftmost z = z
    | otherwise    = leftmost $ left z

-- | Move zipper to the rightmost position.
rightmost :: Zipper a -> Zipper a
rightmost z
    | isRightmost z = z
    | otherwise     = rightmost $ right z

-- | Move zipper one tooth to the left cycling to the end when at the beginning.
leftCycle :: Zipper a -> Zipper a
leftCycle z
    | isLeftmost z = rightmost z
    | otherwise    = left z

-- | Move zipper one tooth to the right cycling to the beginning when at the end.
rightCycle :: Zipper a -> Zipper a
rightCycle z
    | isRightmost z = leftmost z
    | otherwise     = right z

