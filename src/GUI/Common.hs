module GUI.Common where

import Delude
import Engine
import Engine.Layout.Types

import Types.GUI
import GUI.Style

simpleBox :: BoxDesc -> Layout -> Layout
simpleBox d = Layout_Box d . (:[])

fillBox :: Layout -> Layout
fillBox = Layout_Box d . (:[])
    where
    d = def
        & size.width    .~ (1 @@ fill)
        & size.height   .~ (1 @@ fill)

menuBox :: MenuBoxOpts -> Layout -> Layout
menuBox opts cont = Layout_Box desc [ simpleLineupV ins ]
    where
    desc = def
        & size              .~ (opts^.size)
        & boxAlign          .~ Center
        & border.each.width .~ baseBorderWidth
        & border.each.color .~ baseBorderColor
        & color             .~ baseBackgroundColor

    ins = [ tit, contBox ]

    tit = Layout_Box boxDesc [ simpleText $ opts^.title ]
    boxDesc = def
        & size.width  .~ (1 @@ fill)
        & size.height .~ (34 @@ px)
        & border.bottom.width .~ 1
        & border.bottom.color .~ baseBorderColor
        & padding.each        .~ basePadding

    contBox = Layout_Box contDesc [ cont ]
    contDesc = def
        & size.width    .~ (1 @@ fill)
        & size.height   .~ (1 @@ fill)
        & padding.each  .~ basePadding

simpleLineupV :: [Layout] -> Layout
simpleLineupV = Layout_Lineup (set direction Vertical def)

simpleLineupH :: [Layout] -> Layout
simpleLineupH = Layout_Lineup (set direction Horizontal def)

simpleText :: Text -> Layout
simpleText = colorText textPrimaryColor

colorText :: AlphaColor -> Text -> Layout
colorText col txt = Layout_Box bd [Layout_Text td [RichText_Span f txt]]
    where
    f = makeFontStyle baseFontHierarchy baseFontSize & color .~ col
    td = def & boxAlign .~ TopLeft
    bd = def
        & boxAlign      .~ TopLeft
        & size.width    .~ (1 @@ fill)
        & size.height   .~ (30 @@ px)

