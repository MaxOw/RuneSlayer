module GUI.Layout where

import Delude
import Engine.Layout.Alt hiding (left)
import Engine.FontsManager.Types (FontStyle)
import Types.GUI
import Types.EntityAction (AttackMode(..))

import qualified Color

--------------------------------------------------------------------------------

layout_statusPanel :: StatusDesc -> Layout
layout_statusPanel s
    = container txt
    & padding.each   .~ 10 @@ px
    & padding.right  .~ 14 @@ px
    where
    txt = textline fs msg & align .~ BottomRight
    fs = makeFs 14 warningColor

    msg = hr <> am
    hr = bool "" "!" $ s^.ff#hostilesInRange
    am = case s^.ff#attackMode of
        AttackMode_Manual -> "M"
        AttackMode_Auto   -> "A"

--------------------------------------------------------------------------------

layout_offensiveSlotsPanel :: SlotsPanelDesc -> Layout
layout_offensiveSlotsPanel desc = vseprel (8 @@ px)
    [ (1 @@ fill, question)
    , (30 @@ px, answer)
    , (30 @@ px, runeload)
    ]
    & padding.each .~ basePadding
    & size.width   .~ (350 @@ px)
    & size.height  .~ (200 @@ px)
    & align        .~ BottomLeft
    where
    showIf x = if desc^.ff#showQuery then x else def
    question = showIf $ composition
        [ fillColorA bg
        , text fs (desc^.ff#queryText)
            & align .~ TopLeft
            & padding.each .~ 4 @@ px
        ]

    answer = showIf $ border1 Color.gray $ composition
        [ fillColorA bg
        , textline fs (desc^.ff#answerText <> "▏")
            & align .~ MiddleLeft
            & padding.each .~ 8 @@ px
            & padding.left .~ 20 @@ px
        ]

    runeload = hseprel (8 @@ px) runes
        & align .~ TopLeft

    runes = map ((30 @@ px,) . rune) (desc^.slots)

    rune r = border1 Color.gray $ fillColor c
        & size.height .~ pct @@ fill
        & align       .~ BottomCenter
        where
        pct = r^.ff#percent
        c | pct <= 0  = Color.gray
          | pct <  1  = Color.red
          | otherwise = Color.green

    fs = makeFs 10 Color.gray
    bg = Color.withOpacity Color.lightgray 0.4

--------------------------------------------------------------------------------

makeFs :: Int -> Color -> FontStyle
makeFs s c = makeFontStyle ["Arial", "SourceHanSerif"] s
    & color .~ Color.opaque c

basePadding :: Sizing
basePadding = 20 @@ px

warningColor :: Color
warningColor = Color.red

