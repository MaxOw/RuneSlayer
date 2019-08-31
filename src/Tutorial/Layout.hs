module Tutorial.Layout where

import Delude
import Engine.Layout.Alt hiding (left)
import GUI.Layout.Common
import Types.Tutorial

import qualified GUI.Style as Style
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

    con = textStyled (map renderContentPart $ p^.content)
        & align .~ TopLeft

    ft = makeFs 12 Color.darkgray

    fc = makeFs 10 Color.gray
    fk = makeFs 10 Color.white
    fs = makeFsAC 10 Style.textHintColor

    renderContentPart c = (toFontStyle c, c^.ff#text)
    toFontStyle c = case c^.ff#contentType of
         ContentType_Text -> fc
         ContentType_Keys -> if c^.ff#satisfied then fs else fk
         ContentType_Task -> if c^.ff#satisfied then fs else fk

