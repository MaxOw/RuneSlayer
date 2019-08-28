module Tutorial.Layout where

import Delude
import Engine.Layout.Alt hiding (left)
import GUI.Layout.Common
import Types.Tutorial (TutorialPage(..))

import qualified Color

--------------------------------------------------------------------------------

layout_tutorialPage :: TutorialPage -> Layout
layout_tutorialPage p = box ins
    & align        .~ TopRight
    & padding.each .~  20 @@ px
    & width        .~ 550 @@ px
    & height       .~ 250 @@ px
    -- & width  .~ 0.4 @@ fill
    -- & height .~ 0.4 @@ fill
    where
    ins = vrel
        [ (40 @@ px  , tit)
        , (1  @@ fill, con)
        ] & padding.each .~ 8 @@ px
          & padding.top  .~ 6 @@ px

    tit = textline ft (p^.title)   & align .~ TopLeft
    con = text     fc (p^.content) & align .~ TopLeft

    ft = makeFs 12 Color.darkgray
    fc = makeFs 10 Color.gray
