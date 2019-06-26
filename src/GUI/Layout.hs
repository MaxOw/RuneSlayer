module GUI.Layout where

import Delude
import Engine.Layout.Alt hiding (left)
import Engine.FontsManager.Types (FontStyle)
import Types.GUI
import Types.EntityAction (AttackMode(..))
import Types.GameState (GameOverScreen)
import Text.Printf

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

layout_healthStatus :: HealthStatusDesc -> Layout
layout_healthStatus s
    = container txt
    & padding.each .~ 10 @@ px
    where
    txt = textline fs msg & align .~ BottomCenter
    fs = makeFs 14 warningColor

    msg = toText @String $ printf "%d/%d HP"
        (s^.health._Wrapped)
        (s^.maxHealth._Wrapped)

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

layout_offensiveSlotsPanel :: SlotsPanelDesc -> Layout
layout_offensiveSlotsPanel = layout_slotsPanel BottomLeft  Color.green

layout_defensiveSlotsPanel :: SlotsPanelDesc -> Layout
layout_defensiveSlotsPanel = layout_slotsPanel BottomRight Color.darkgray

layout_slotsPanel :: BoxAlign -> Color -> SlotsPanelDesc -> Layout
layout_slotsPanel agn fcol desc = vseprel (8 @@ px)
    [ (1 @@ fill, question)
    , (30 @@ px, answer)
    , (30 @@ px, runeload)
    ]
    & padding.each .~ basePadding
    & size.width   .~ (350 @@ px)
    & size.height  .~ (200 @@ px)
    & align        .~ agn
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
          | otherwise = fcol

    fs = makeFs 10 Color.black
    bg = Color.withOpacity Color.lightgray 0.6

--------------------------------------------------------------------------------

makeFs :: Int -> Color -> FontStyle
makeFs s c = makeFontStyle ["Arial", "SourceHanSerif"] s
    & color .~ Color.opaque c

makeFsa :: Int -> Color -> Float -> FontStyle
makeFsa s c a = makeFontStyle ["Arial", "SourceHanSerif"] s
    & color .~ Color.withOpacity c a

basePadding :: Sizing
basePadding = 20 @@ px

warningColor :: Color
warningColor = Color.red

