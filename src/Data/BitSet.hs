module Data.BitSet
    ( BitSet, BitSet32, BitSet64
    , fromList
    , null
    , member
    , insert
    , delete
    , toggle
    , intersection
    , union
    ) where

import Relude hiding (fromList, empty, null)
import Data.Aeson
import Data.Default
import Data.Bits
import Dhall (Interpret, autoWith)

--------------------------------------------------------------------------------

newtype BitSet b e = BitSet b

type BitSet32 = BitSet Word32
type BitSet64 = BitSet Word64

--------------------------------------------------------------------------------

instance Bits b => Default (BitSet b e) where def = empty
instance (FromJSON e, Enum e, Bits b) => FromJSON (BitSet b e) where
    parseJSON = fmap fromList . parseJSON
instance (Interpret e, Enum e, Bits b) => Interpret (BitSet b e) where
    autoWith = fmap fromList . autoWith

--------------------------------------------------------------------------------

empty :: Bits b => BitSet b e
empty = BitSet zeroBits

null :: (Eq b, Bits b) => BitSet b e -> Bool
null (BitSet b) = b == zeroBits

fromList :: (Enum e, Bits b) => [e] -> BitSet b e
fromList = foldl' (flip insert) empty

member :: (Enum e, Bits b) => e -> BitSet b e -> Bool
member e (BitSet b) = testBit b (fromEnum e)

insert :: (Enum e, Bits b) => e -> BitSet b e -> BitSet b e
insert e (BitSet b) = BitSet $ setBit b (fromEnum e)

delete :: (Enum e, Bits b) => e -> BitSet b e -> BitSet b e
delete e (BitSet b) = BitSet $ clearBit b (fromEnum e)

toggle :: (Enum e, Bits b) => e -> BitSet b e -> BitSet b e
toggle e (BitSet b) = BitSet $ complementBit b (fromEnum e)

intersection :: Bits b => BitSet b e -> BitSet b e -> BitSet b e
intersection (BitSet a) (BitSet b) = BitSet (a .&. b)

union :: Bits b => BitSet b e -> BitSet b e -> BitSet b e
union (BitSet a) (BitSet b) = BitSet (a .|. b)
