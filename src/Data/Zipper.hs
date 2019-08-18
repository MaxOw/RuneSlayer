module Data.Zipper
    ( Zipper
    , fromList, empty
    , null, isLeftmost, isRightmost
    , focus
    , left, right
    ) where

import Relude hiding (fromList, empty, null)

-- | List zipper data type.
data Zipper a = Zipper [a] [a]

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
