{-# OPTIONS_GHC -fno-warn-orphans #-}
module Delude
    ( module All

    , Transformable2D
    , Mat4, AlphaColor
    , unwrap
    , isPrism
    , makeFieldsCustom

    , boundedRange

    , monoidJust
    , nothingFalse
    , nothingFalse2

    , toggleSet
    ) where

import Relude         as All
import Control.Lens   as All hiding (uncons)
import Data.Default   as All
import Linear         as All hiding (trace, transpose, identity, rotate)
import Diagrams.Angle as All ((@@))
import Data.Bimap     as All (Bimap)

import qualified Data.Bimap as Bimap

import qualified Data.Vector as Vector
import qualified Data.Set    as Set

import Diagrams.Core (InSpace, Transformable)
import Engine (Mat4, AlphaColor)
import Engine.TH

import HasField as All

-- import Linear       as All hiding (trace, identity, transpose)

unwrap :: Rewrapped a a => a -> Unwrapped a
unwrap = view _Wrapped

isPrism :: APrism s t a b -> s -> Bool
isPrism p = not . isn't p

type Transformable2D t = (InSpace V2 Float t, Transformable t)

--------------------------------------------------------------------------------

instance Default a => Default (V2 a)
instance Default Text where def = ""
instance Default Bool where def = False
instance Default (Bimap a b) where def = Bimap.empty
instance Default (Vector.Vector a) where def = Vector.empty

--------------------------------------------------------------------------------

boundedRange :: (Bounded a, Enum a) => [a]
boundedRange = [minBound .. maxBound]

--------------------------------------------------------------------------------

monoidJust :: Monoid m => Maybe x -> (x -> m) -> m
monoidJust Nothing  _ = mempty
monoidJust (Just x) f = f x

nothingFalse :: Maybe a -> (a -> Bool) -> Bool
nothingFalse Nothing  _ = False
nothingFalse (Just a) f = f a

nothingFalse2 :: Maybe a -> Maybe b -> (a -> b -> Bool) -> Bool
nothingFalse2 Nothing        _  _ = False
nothingFalse2       _  Nothing  _ = False
nothingFalse2 (Just a) (Just b) f = f a b

toggleSet :: Ord a => a -> Set a -> Set a
toggleSet a s
    | Set.member a s = Set.delete a s
    | otherwise      = Set.insert a s
