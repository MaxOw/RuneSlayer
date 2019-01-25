module GUI.Common where

import Delude
import Engine
import Engine.Layout.Types

import Types.GUI
import GUI.Style

simpleBox :: BoxDesc -> Layout -> Layout
simpleBox d = layoutBox d . (:[])

fillBox :: Layout -> Layout
fillBox = layoutBox d . (:[])
    where
    d = def
        & size.width    .~ (1 @@ fill)
        & size.height   .~ (1 @@ fill)

menuBox :: MenuBoxOpts -> Layout -> Layout
menuBox opts cont = layoutBox desc [ simpleLineupV ins ]
    where
    desc = def
        & size              .~ (opts^.size)
        & boxAlign          .~ Center
        & border.each.width .~ baseBorderWidth
        & border.each.color .~ baseBorderColor
        & color             .~ baseBackgroundColor

    ins = [ tit, contBox ]

    tit = layoutBox boxDesc [ simpleText $ opts^.title ]
    boxDesc = def
        & size.width  .~ (1 @@ fill)
        & size.height .~ (34 @@ px)
        & border.bottom.width .~ 1
        & border.bottom.color .~ baseBorderColor
        & padding.each        .~ basePadding

    contBox = layoutBox contDesc [ cont ]
    contDesc = def
        & size.width    .~ (1 @@ fill)
        & size.height   .~ (1 @@ fill)

withTitle :: Text -> Layout -> Layout
withTitle t lay = simpleLineupV [ tit, fillBox lay ]
    where
    tit = layoutBox boxDesc [ simpleText t ]
    boxDesc = def
        & size.width  .~ (1 @@ fill)
        & size.height .~ (34 @@ px)
        & border.bottom.width .~ 1
        & border.bottom.color .~ baseBorderColor
        & padding.each        .~ basePadding

simpleLineupV :: [Layout] -> Layout
simpleLineupV = layoutLineup (set direction Vertical def)

simpleLineupH :: [Layout] -> Layout
simpleLineupH = layoutLineup (set direction Horizontal def)

simpleText :: Text -> Layout
simpleText = colorText textPrimaryColor

colorText :: AlphaColor -> Text -> Layout
colorText col txt = colorTextList [(col, txt)]

colorTextList :: [(AlphaColor, Text)] -> Layout
colorTextList cs = layoutBox bd [layoutText td $ map f cs]
    where
    f (c, t) = RichText_Span (fnt c) t
    fnt c = makeFontStyle baseFontHierarchy baseFontSize & color .~ c
    td = def & boxAlign .~ TopLeft
    bd = def
        & boxAlign      .~ TopLeft
        & size.width    .~ (1 @@ fill)
        & size.height   .~ (30 @@ px)

overlayLayouts :: [Layout] -> Layout
overlayLayouts = layoutBox d
    where
    d = def
        & size.width    .~ (1 @@ fill)
        & size.height   .~ (1 @@ fill)

