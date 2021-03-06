module GUI.Style where

-- import qualified Engine
import Delude
import Engine
import Engine.Layout.Types
-- (FontFamilyName)

import Data.Colour as Color
-- import Data.Colour.Names as Color

import qualified Solarized

baseFontSize :: FontSize
baseFontSize = 10

-- TODO: Make this configurable from dhall
baseFontHierarchy :: [FontFamilyName]
baseFontHierarchy = ["LiberationSans", "SourceHanSerif"]

baseFontStyle :: FontStyle
baseFontStyle = makeFontStyle baseFontHierarchy baseFontSize
    & color .~ textPrimaryColor

baseBorderWidth :: AbsoluteSize
baseBorderWidth = 1

basePadding :: AbsoluteSize
basePadding = 4

smode :: Solarized.SolarizedMode
smode = Solarized.ModeDark
-- smode = Solarized.ModeLight

baseBorderColor :: AlphaColor
-- baseBorderColor = Color.opaque $ Solarized.secondaryContent smode
baseBorderColor = Color.opaque $ Solarized.primaryContent smode

baseContentColor :: AlphaColor
baseContentColor = Color.opaque $ Solarized.primaryContent smode

baseBackgroundColor :: AlphaColor
baseBackgroundColor = Color.opaque $ Solarized.background smode

textPrimaryColor :: AlphaColor
textPrimaryColor = Color.opaque $ Solarized.primaryContent smode

textSecondaryColor :: AlphaColor
textSecondaryColor = Color.opaque $ Solarized.secondaryContent smode

textHintColor :: AlphaColor
textHintColor = Color.opaque $ Solarized.yellow

textHintHighlightColor :: AlphaColor
textHintHighlightColor = Color.opaque $ Solarized.red

textWarningColor :: AlphaColor
textWarningColor = Color.opaque $ Solarized.red

textFocusColor :: AlphaColor
textFocusColor = Color.opaque $ Solarized.yellow

baseBox :: BoxDesc
baseBox = def
    & border.each.width .~ baseBorderWidth
    & border.each.color .~ baseBorderColor
    & padding.each      .~ basePadding
    & color             .~ baseBackgroundColor

