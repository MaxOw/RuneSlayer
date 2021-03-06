{-# OPTIONS_GHC -fno-warn-orphans #-}
module Delude
    ( module All

    , Transformable2D
    , Mat4
    , unwrap
    , isPrism
    , makeFieldsCustom
    , customOptionsJSON
    , defaultToJSONKey
    , defaultFromJSONKey

    , boundedRange

    , monoidJust
    , nothingFalse
    , nothingFalse2
    , fromMaybe2
    , bindMaybeM

    , toggleSet
    , require

    , sortVia
    , sortSelectVia

    , capitalize
    , splitUpper
    , camelToHuman

    , whenChanged_

    , nextStop, precStop
    , (.:)
    ) where

import Relude         as All
import Control.Lens   as All hiding (uncons, (??))
import Data.Default   as All
import Linear         as All hiding (trace, transpose, identity, rotate)
import Linear.Affine  as All (Point (..))
import Diagrams.Angle as All ((@@), turn)
import Data.Bimap     as All (Bimap)
import Data.Aeson     as All
    ( ToJSON(..), FromJSON(..), ToJSONKey(..), FromJSONKey(..)
    , genericToEncoding, genericParseJSON)

import Data.BitSet    as All (BitSet32, BitSet64)

-- import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson

import Relude.Extra.Enum as All (next, prec)

import qualified Data.Aeson as Aeson
import Engine.Common.Types
import Engine.Graphics.Types as All (Color, AlphaColor)

import qualified Data.List  as List
import qualified Data.Map   as Map
import qualified Data.Bimap as Bimap
import qualified Data.HashMap.Strict as HashMap

import qualified Data.Vector as Vector
import qualified Data.Set    as Set

import qualified Data.IxSet.Typed as IxSet

import Data.Char (isUpper, toUpper)
import Data.List.Split

import Diagrams.Core (InSpace, Transformable)
import Engine (Mat4)
import Engine.TH

import HasField as All

import Data.Hashable (hash)
import System.IO.Unsafe (unsafePerformIO)

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

instance ToJSON a => ToJSON (V2 a)
instance ToJSON a => ToJSON (Size a)
instance FromJSON a => FromJSON (V2 a)
instance FromJSON a => FromJSON (Size a)

instance ToJSON a => ToJSON (Rect a) where
    toEncoding = Aeson.genericToEncoding customOptionsJSON
instance FromJSON a => FromJSON (Rect a) where
    parseJSON = Aeson.genericParseJSON customOptionsJSON

instance IxSet.Indexable a b => Default (IxSet.IxSet a b) where def = mempty

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

defaultToJSONKey :: ToJSON a => Aeson.ToJSONKeyFunction a
defaultToJSONKey = Aeson.ToJSONKeyText f (Aeson.text . f)
    where f = decodeUtf8 . Aeson.encode

defaultFromJSONKey :: FromJSON a => Aeson.FromJSONKeyFunction a
defaultFromJSONKey = Aeson.FromJSONKeyTextParser (parseJSON . Aeson.String)

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

fromMaybe2 :: c -> Maybe a -> Maybe b -> (a -> b -> c) -> c
fromMaybe2 c Nothing        _  _ = c
fromMaybe2 c       _  Nothing  _ = c
fromMaybe2 _ (Just a) (Just b) f = f a b

bindMaybeM :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
bindMaybeM ma mb = ma >>= maybe (return Nothing) mb

toggleSet :: Ord a => a -> Set a -> Set a
toggleSet a s
    | Set.member a s = Set.delete a s
    | otherwise      = Set.insert a s

require :: HasCallStack => Text -> Maybe a -> a
require _   (Just a) = a
require msg Nothing  = error msg

--------------------------------------------------------------------------------

sortVia :: Ord e => (a -> e) -> [e] -> [a] -> [a]
sortVia f ms = sortOn (fromMaybe 0 . flip Map.lookup emap . f)
    where
    emap = Map.fromList $ zip ms [1 :: Int ..]

sortSelectVia :: Ord e => (a -> e) -> [e] -> [a] -> [a]
sortSelectVia f ms
    = map snd
    . sortOn fst
    . mapMaybe (\x -> (,x) <$> Map.lookup (f x) emap)
    where
    emap = Map.fromList $ zip ms [1 :: Int ..]

capitalize :: String -> String
capitalize = over (ix 0) toUpper

splitUpper :: String -> [String]
splitUpper = split . keepDelimsL $ whenElt isUpper

camelToHuman :: String -> String
camelToHuman = List.unwords . map capitalize . splitUpper

--------------------------------------------------------------------------------

{-# NOINLINE changeCacheRef #-}
changeCacheRef :: IORef (HashMap String Int)
changeCacheRef = unsafePerformIO $ newIORef mempty

whenChanged_ :: (HasCallStack, Hashable a, MonadIO m) => a -> (a -> m ()) -> m ()
whenChanged_ newValue doAction = do
    let cname = prettyCallStack callStack
    let newHash = hash newValue
    cmap <- readIORef changeCacheRef
    case HashMap.lookup cname cmap of
        Nothing      -> valueChanged cname newHash
        Just oldHash -> if oldHash /= newHash
            then valueChanged cname newHash
            else return ()
    where
    valueChanged cname newHash = do
        -- putStrLn cname
        atomicModifyIORef' changeCacheRef $ \c ->
            (HashMap.insert cname newHash c, ())
        doAction newValue

--------------------------------------------------------------------------------

nextStop :: (Eq a, Bounded a, Enum a) => a -> a
nextStop a = if a == maxBound then a else succ a

precStop :: (Eq a, Bounded a, Enum a) => a -> a
precStop a = if a == minBound then a else pred a

--------------------------------------------------------------------------------

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

