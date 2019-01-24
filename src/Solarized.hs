{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Solarized where

import Delude
-- import Data.Colour
import Data.Colour.SRGB
-- import Data.Colour.Names

data SolarizedMode
   = ModeLight
   | ModeDark

default (Float)

base03  = sRGB24read "002b36"
base02  = sRGB24read "073642"
base01  = sRGB24read "586e75"
base00  = sRGB24read "657b83"
base0   = sRGB24read "839496"
base1   = sRGB24read "93a1a1"
base2   = sRGB24read "eee8d5"
base3   = sRGB24read "fdf6e3"

yellow  = sRGB24read "b58900"
orange  = sRGB24read "cb4b16"
red     = sRGB24read "dc322f"
magenta = sRGB24read "d33682"
violet  = sRGB24read "6c71c4"
blue    = sRGB24read "268bd2"
cyan    = sRGB24read "2aa198"
green   = sRGB24read "859900"

primaryContent ModeDark  = base0
primaryContent ModeLight = base00

secondaryContent ModeDark  = base01
secondaryContent ModeLight = base1

emphasizedContent ModeDark  = base1
emphasizedContent ModeLight = base01

background ModeDark  = base03
background ModeLight = base3

backgroundHighlight ModeDark  = base02
backgroundHighlight ModeLight = base2
