module GUI.Layout (module GUI.Layout) where

import Delude
import qualified Data.Text as Text
import Engine.Layout.Alt hiding (left)
import Types.GUI
import Types.EntityAction (AttackMode(..))
import Types.GameState (GameOverScreen)
import Text.Printf

import GUI.Layout.Inventory as GUI.Layout
import GUI.Layout.Common    as GUI.Layout
import qualified GUI.Style as Style

import qualified Color

--------------------------------------------------------------------------------

layout_actionableLabel :: Text -> Text -> V2 Float -> Layout
layout_actionableLabel key msg loc = absolute locInPixels con
    & size.height .~ 20 @@ px
    & size.width  .~ 300 @@ px
    where
    con = hrel [ (120 @@ px, klab), (1 @@ fill, txt) ] & align .~ Center
    locInPixels = fmap ((@@ px) . realToFrac) loc
    txt  = textline fs msg & align .~ MiddleLeft & padding.left .~ 20 @@ px
    klab = textline fk key & align .~ MiddleRight
    fs = makeFs 12 Color.red
    fk = makeFs 12 Color.white

layout_statusPanel :: Status -> Layout
layout_statusPanel s
    = composition [ healthAndRunes, statusText ]
    & padding.each   .~ 10 @@ px
    & padding.right  .~ 14 @@ px
    where
    statusText = textline fs msg & align .~ BottomRight
    fs = makeFs 14 warningColor

    msg = mconcat [hr, ir, am]
    hr = bool "" "!" $ s^.ff#hostilesInRange
    ir = bool "" "I" $ s^.ff#itemsInRange
    am = case s^.ff#attackMode of
        AttackMode_Manual -> "M"
        AttackMode_Auto   -> "A"

    healthAndRunes = hcat [ healthL, runesL ]
        & width .~ 300 @@ px

    healthL = layout_healthPointsStatus $ s^.ff#health
    runesL  = layout_runicPointsStatus  $ s^.ff#runes

--------------------------------------------------------------------------------

layout_pointsStatus :: String -> Color -> StatusPoints -> Layout
layout_pointsStatus f c s = txt
    where
    txt = textline fs msg & align .~ BottomCenter
    fs = makeFs 14 c

    msg = toText @String $ printf f (s^.ff#points) (s^.ff#maxPoints)

layout_healthPointsStatus :: StatusPoints -> Layout
layout_healthPointsStatus = layout_pointsStatus "%d/%d HP" warningColor

layout_runicPointsStatus :: StatusPoints -> Layout
layout_runicPointsStatus = layout_pointsStatus "%d/%d RP" Color.blue

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
layout_storyDialog s = box ins
    & align .~ BottomCenter
    & padding.bottom .~ 100 @@ px
    & padding.left   .~ 100 @@ px
    & padding.right  .~ 100 @@ px
    & height .~ 0.4 @@ fill
    where
    ins = vrel
        [ (40 @@ px  , tit)
        , (1  @@ fill, con)
        , (40 @@ px  , prc)
        ] & padding.each .~ 8 @@ px
          & padding.top  .~ 6 @@ px

    nextPageText = "Press " <> s^.ff#nextPageKey <> " to proceed..."

    tit = textline ft (s^.title)   & align .~ TopLeft
    con = text     fc (s^.content) & align .~ TopLeft
    prc = textline fp nextPageText & align .~ BottomRight

    ft = makeFs 12 Color.darkgray
    fc = makeFs 10 Color.gray
    fp = makeFs 8 warningColor

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
    [ (30 @@ px, result)
    , (1 @@ fill, question)
    , (30 @@ px, answer)
    ]
    & padding.each .~ basePadding
    & size.width   .~ (350 @@ px)
    & size.height  .~ (240 @@ px)
    & align        .~ BottomLeft
    where
    question = composition
        [ fillColorA bg
        , text fs (desc^.ff#queryText)
            & align .~ TopLeft
            & padding.each .~ 4 @@ px
        ]

    answer = border1 Color.gray $ composition
        [ fillColorA bg
        , textlineNoCache fs (desc^.ff#answerText <> "▏")
            & align .~ MiddleLeft
            & padding.each .~ 8 @@ px
            & padding.left .~ 20 @@ px
        ]

    result = case desc^.ff#prevResult of
        Nothing -> def
        Just r  -> renderResult r

    renderResult r = if r^.ff#result
        then resultBox Color.green "Correct!"
        else resultBox Color.red   "Wrong!"

    resultBox c t = border1 c $ composition
        [ fillColorA bg
        , textline (fsC c) t
            & align .~ MiddleLeft
            & padding.each .~ 8 @@ px
            & padding.left .~ 20 @@ px
        ]

    fs = makeFs 10 Color.black
    fsC = makeFs 10
    bg = Color.withOpacity Color.lightgray 0.6

--------------------------------------------------------------------------------

layout_runesStatus :: RunesStatus -> Layout
layout_runesStatus rs = statusBox ins
    where
    ins = hcat [ leftside, rightside ]

    leftside
        = set align TopLeft
        . setDefaultPadding
        . vlist (30 @@ px)
        . map (layout_runeStatus mm)
        $ rs^.ff#runes

    mm = rs^.ff#maxMastery

    rightside = mempty

layout_runeStatus :: Int -> RuneStatus -> Layout
layout_runeStatus mm r = hseprel (8 @@ px)
    [ (100 @@ px  , rname)
    , (  1 @@ fill, answers)
    , (100 @@ px  , progressBar pct Color.green)
    , (40  @@ px  , mastery) ]
    where
    rname   = textline fs  (r^.name)   & align .~ MiddleLeft
    answers = textline fsi answersText & align .~ MiddleLeft
    mastery = textline fs  masteryText & align .~ MiddleRight
    answersText = Text.unwords $ r^.ff#answers
    masteryText = show (r^.ff#mastery) <> "/" <> show mm
    pct = fromIntegral (r^.ff#mastery) / fromIntegral mm
    -- textStyled [(fs, r^.name), (fsi, mconcat $ r^.ff#answers)]
    fs  = makeFsAC 10 Style.textPrimaryColor
    fsi = makeFsAC 10 Style.textSecondaryColor

