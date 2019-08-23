module GUI.Layout (module GUI.Layout) where

import Delude
import Engine.Layout.Alt hiding (left)
import Types.GUI
import Types.EntityAction (AttackMode(..))
import Types.GameState (GameOverScreen)
import Text.Printf

import GUI.Layout.Inventory as GUI.Layout
import GUI.Layout.Common    as GUI.Layout

import qualified Color

--------------------------------------------------------------------------------

layout_statusPanel :: Status -> Layout
layout_statusPanel s
    = composition [ healthAndRunes, statusText ]
    & padding.each   .~ 10 @@ px
    & padding.right  .~ 14 @@ px
    where
    statusText = textline fs msg & align .~ BottomRight
    fs = makeFs 14 warningColor

    msg = hr <> am
    hr = bool "" "!" $ s^.ff#hostilesInRange
    am = case s^.ff#attackMode of
        AttackMode_Manual -> "M"
        AttackMode_Auto   -> "A"

    healthAndRunes = hcat [ healthL, runesL ]
        & width .~ 300 @@ px

    healthL = layout_healthStatus $ s^.ff#health
    runesL  = layout_runicStatus  $ s^.ff#runes

--------------------------------------------------------------------------------

layout_pointsStatus :: String -> Color -> StatusPoints -> Layout
layout_pointsStatus f c s = txt
    where
    txt = textline fs msg & align .~ BottomCenter
    fs = makeFs 14 c

    msg = toText @String $ printf f (s^.ff#points) (s^.ff#maxPoints)

layout_healthStatus :: StatusPoints -> Layout
layout_healthStatus = layout_pointsStatus "%d/%d HP" warningColor

layout_runicStatus :: StatusPoints -> Layout
layout_runicStatus = layout_pointsStatus "%d/%d RP" Color.blue

--------------------------------------------------------------------------------

layout_actionsMenu :: [ActionHint] -> Layout
layout_actionsMenu ls = vrel hintLines
    & align .~ MiddleLeft
    & padding.each .~ basePadding
    where
    hintLines = zip (repeat $ 20 @@ px) (map toLine ls)
    toLine l = textline fs (l^.ff#actionHint <> " " <> l^.ff#actionName)
        & align .~ BottomLeft
    fs = makeFs 12 warningColor

--------------------------------------------------------------------------------

layout_storyDialog :: StoryDialog -> Layout
layout_storyDialog s = border1 Color.gray cnt
    & align .~ BottomCenter
    & padding.bottom .~ 100 @@ px
    & padding.left   .~ 100 @@ px
    & padding.right  .~ 100 @@ px
    & height .~ 0.4 @@ fill
    where
    cnt = composition
        [ fillColorA bg
        , ins
        ]

    ins = vrel
        [ (40 @@ px  , tit)
        , (1  @@ fill, con)
        , (40 @@ px  , prc)
        ] & padding.each .~ 8 @@ px
          & padding.top  .~ 6 @@ px

    nextPageText = "Press " <> s^.ff#nextPageKey <> " to proceed..."

    tit = textline ft (s^.title)   & align .~ TopLeft
    con = textline fc (s^.content) & align .~ TopLeft
    prc = textline fp nextPageText & align .~ BottomRight

    ft = makeFs 12 Color.darkgray
    fc = makeFs 10 Color.gray
    fp = makeFs 8 warningColor
    bg = Color.withOpacity Color.black 0.6

--------------------------------------------------------------------------------

layout_gameOverScreen :: GameOverScreen -> Layout
layout_gameOverScreen gs = vrel
    [ (80 @@ px, text_youDied)
    , (60 @@ px, text_pressAnyKey) ]
    & align .~ MiddleCenter
    where
    fm = makeFsa 48 warningColor (max 0 $ min 1 $ gs^.timer._Wrapped)
    fb = makeFsa 14 warningColor (if gs^.ff#pressAnyKey then 1 else 0)

    text_youDied     = textline fm "You Died"      & align .~ BottomCenter
    text_pressAnyKey = textline fb "Press any key" & align .~ TopCenter

--------------------------------------------------------------------------------

layout_runicMode :: RunicMode -> Layout
layout_runicMode desc = vseprel (8 @@ px)
    [ (1 @@ fill, question)
    , (30 @@ px, answer)
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

    fs = makeFs 10 Color.black
    bg = Color.withOpacity Color.lightgray 0.6

