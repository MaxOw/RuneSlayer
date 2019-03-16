{-# OPTIONS_GHC -fno-warn-orphans #-}
module Delude
    ( module All

    , Transformable2D
    , Mat4, AlphaColor
    , unwrap
    , isPrism
    , makeFieldsCustom
    , customOptionsJSON

    , boundedRange

    , monoidJust
    , nothingFalse
    , nothingFalse2

    , toggleSet
    ) where

import Relude         as All
import Control.Lens   as All hiding (uncons, (??))
import Data.Default   as All
import Linear         as All hiding (trace, transpose, identity, rotate)
import Linear.Affine  as All (Point (..))
import Diagrams.Angle as All ((@@))
import Data.Bimap     as All (Bimap)
import Data.Aeson     as All
    (ToJSON(..), FromJSON(..), genericToEncoding, genericParseJSON)

import qualified Data.Aeson as Aeson
import Engine.Common.Types

import qualified Data.Bimap as Bimap
import qualified Data.HashMap.Strict as HashMap

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
instance Default (HashMap a b) where def = HashMap.empty

deriving instance Generic (Rect a)
instance ToJSON a => ToJSON (V2 a)
instance ToJSON a => ToJSON (Size a)
instance FromJSON a => FromJSON (V2 a)
instance FromJSON a => FromJSON (Size a)

instance ToJSON a => ToJSON (Rect a) where
    toEncoding = Aeson.genericToEncoding customOptionsJSON
instance FromJSON a => FromJSON (Rect a) where
    parseJSON = Aeson.genericParseJSON customOptionsJSON

--------------------------------------------------------------------------------

customOptionsJSON :: Aeson.Options
customOptionsJSON = Aeson.defaultOptions
    { Aeson.fieldLabelModifier = dropPrefix
    , Aeson.constructorTagModifier = dropPrefix
    , Aeson.sumEncoding = Aeson.ObjectWithSingleField
    }
    where
    dropPrefix x = if any (=='_') x then drop 1 $ dropWhile (/='_') x else x

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
