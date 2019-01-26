{-# OPTIONS_GHC -fno-warn-orphans #-}
module Delude
    ( module All

    , Transformable2D
    , Mat4, AlphaColor
    , unwrap
    , isPrism
    , makeFieldsCustom
    ) where

import Relude         as All
import Control.Lens   as All hiding (uncons)
import Data.Default   as All
import Linear         as All hiding (trace, transpose, identity, rotate)
import Diagrams.Angle as All ((@@))
import Data.Bimap     as All (Bimap)

import qualified Data.Bimap as Bimap

import Diagrams.Core (InSpace, Transformable)
import Engine (Mat4, AlphaColor)
import Engine.TH

import HasField as All

-- import Linear       as All hiding (trace, identity, transpose)

unwrap :: Rewrapped a a => a -> Unwrapped a
unwrap = view _Wrapped

isPrism :: APrism s t a b -> s -> Bool
isPrism p = not . isn't p

type Transformable2D t = (InSpace V2 Double t, Transformable t)

--------------------------------------------------------------------------------

instance Default a => Default (V2 a)
instance Default Text where def = ""
instance Default Bool where def = False
instance Default (Bimap a b) where def = Bimap.empty

