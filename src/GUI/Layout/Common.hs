module GUI.Layout.Common where

import Delude
import Engine.Layout.Alt hiding (left)
import Engine.FontsManager.Types (FontStyle)
import qualified Color

--------------------------------------------------------------------------------

makeFs :: Int -> Color -> FontStyle
makeFs s c = makeFontStyle ["Arial", "SourceHanSerif"] s
    & color .~ Color.opaque c

makeFsa :: Int -> Color -> Float -> FontStyle
makeFsa s c o = makeFontStyle ["Arial", "SourceHanSerif"] s
    & color .~ Color.withOpacity c o

makeFsAC :: Int -> AlphaColor -> FontStyle
makeFsAC s ac = makeFontStyle ["Arial", "SourceHanSerif"] s
    & color .~ ac

basePadding :: Sizing
basePadding = 20 @@ px

warningColor :: Color
warningColor = Color.red

--------------------------------------------------------------------------------

box :: Layout -> Layout
box x = border1 Color.gray $ composition [ fillColorA bg, x ]
    where bg = Color.withOpacity Color.black 0.6

borderSep :: (Sizing, Layout)
borderSep = (1 @@ px, fillColor Color.gray)

catb1 :: ([(Sizing, Layout)] -> Layout) -> [Layout] -> Layout
catb1 f = f . intersperse borderSep . map (1 @@ fill,)

hcatb1 :: [Layout] -> Layout
hcatb1 = catb1 hrel

vcatb1 :: [Layout] -> Layout
vcatb1 = catb1 vrel

vlist :: Sizing -> [Layout] -> Layout
vlist s = vrel . map (s,)

