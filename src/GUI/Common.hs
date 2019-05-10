module GUI.Common where

import Delude hiding (direction)
import Engine
import Engine.Layout.Types

import Types.GUI.Common
import GUI.Style

simpleBox :: BoxDesc -> Layout -> Layout
simpleBox d = layoutBox d . (:[])

progressBarV :: Size Sizing -> AlphaColor -> Float -> Layout
progressBarV ss col pct = layoutBox bdesc [ layoutBox pdesc [] ]
    where
    pdesc = def
        & boxAlign    .~ BottomCenter
        & size.height .~ pct @@ cpct
        & color       .~ col
    bdesc = def
        & size              .~ ss
        & border.each.width .~ baseBorderWidth
        & border.each.color .~ col

menuBox :: MenuBoxOpts -> Layout -> Layout
menuBox opts cont = layoutBox desc [ simpleLineupV ins ]
    where
    desc = def
        & size              .~ (opts^.size)
        & boxAlign          .~ Center
        & border.each.width .~ baseBorderWidth
        & border.each.color .~ baseBorderColor
        & color             .~ baseBackgroundColor

    ins = [ tit, cont ]

    tit = layoutBox boxDesc [ simpleText $ opts^.title ]
    boxDesc = def
        & size.height .~ (34 @@ px)
        & border.bottom.width .~ 1
        & border.bottom.color .~ baseBorderColor
        & padding.each        .~ basePadding

withTitle :: Text -> Layout -> Layout
withTitle t lay = simpleLineupV [ tit, lay ]
    where
    tit = layoutBox boxDesc [ simpleText t ]
    boxDesc = def
        & size.height .~ (34 @@ px)
        & border.bottom.width .~ 1
        & border.bottom.color .~ baseBorderColor
        & padding.each        .~ basePadding

withPadding :: Layout -> Layout
withPadding = simpleBox $ def
    & padding.each .~ basePadding

simpleLineupV :: [Layout] -> Layout
simpleLineupV = layoutLineup (set direction Vertical def)

simpleLineupH :: [Layout] -> Layout
simpleLineupH = layoutLineup (set direction Horizontal def)

simpleText :: Text -> Layout
simpleText = colorText textPrimaryColor

styleText :: FontStyle -> Text -> Layout
styleText fs txt = styleTextList [(fs, txt)]

colorText :: AlphaColor -> Text -> Layout
colorText col txt = colorTextList [(col, txt)]

colorTextList :: [(AlphaColor, Text)] -> Layout
colorTextList = styleTextList . map (over _1 f)
    where
    f c = baseFontStyle & color .~ c

styleTextList :: [(FontStyle, Text)] -> Layout
styleTextList cs = layoutBox bd [layoutText td $ map f cs]
    where
    f (fs, t) = RichText_Span fs t
    td = def & boxAlign .~ TopLeft
    bd = def
        & boxAlign      .~ TopLeft
        & size.height   .~ (30 @@ px)

overlayLayouts :: [Layout] -> Layout
overlayLayouts = layoutBox def

