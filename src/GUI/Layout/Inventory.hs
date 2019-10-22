module GUI.Layout.Inventory where

import Delude
import Text.Printf
import qualified Data.Text as Text

import Types.GUI
import Engine.Layout.Alt hiding (left)
import GUI.Layout.Common
import qualified GUI.Style as Style

import qualified Color

--------------------------------------------------------------------------------

layout_inventory :: Inventory -> Layout
layout_inventory i = box ins
    & align     .~ Center
    & size.each .~ 0.8 @@ fill
    where
    ins = vrel [ (1 @@ fill, topb), borderSep, ( 42 @@ px, botb) ]
    topb = hcatb1 [ leftside, rightside ]
    botb = layout_info $ i^.ff#keys

    leftside  = vcatb1 [ equipmentSlots, description ]
    rightside = vcatb1 $ map layout_container $ i^.ff#containers

    equipmentSlots = layout_selectEntries $ i^.ff#equipment
    description    = fromMaybe def $ layout_description <$> i^.ff#description

layout_info :: [KeyInfo] -> Layout
layout_info
    = set align TopLeft
    . setDefaultPadding
    . textStyled
    . concatMap keyInfo
    where
    keyInfo (KeyInfo k inf) = [(fsk, k), (fsi, " - " <> inf <> "; ")]
    fsk = makeFsAC 10 Style.textPrimaryColor
    fsi = makeFsAC 10 Style.textSecondaryColor

layout_selectEntries :: [SelectEntry] -> Layout
layout_selectEntries
    = set align TopLeft
    . setDefaultPadding
    . vlist (30 @@ px)
    . map layout_selectEntry

layout_selectEntry :: SelectEntry -> Layout
layout_selectEntry entry = addLabel $ hseprel (8 @@ px)
    [ (30 @@ px  , prefixHint)
    , (1  @@ fill, entityName) ]
    where
    addLabel = maybe id (\x y -> hcat [printLabel x, y]) (entry^.label)
    printLabel x = textline ft x & align .~ MiddleLeft

    hintText   = fromString $ fromMaybe "" $ entry^.hint
    -- prefixHint = textline ft hintText & align .~ MiddleRight
    prefixHint = case entry^.prefix of
        Nothing -> colorText Style.textPrimaryColor hintText
        Just pfx -> if Text.isPrefixOf (fromString pfx) hintText
            then prefixTextHighlight pfx
            else colorText Style.textSecondaryColor hintText
    prefixTextHighlight pfx = colorTextList
        [(Style.textHintHighlightColor, textPfx)
        ,(Style.textHintColor         , textRest)]
        where
        textPfx = fromString pfx
        textRest = fromMaybe "" $ Text.stripPrefix textPfx hintText

    colorText c t = textline (makeFsAC 12 c) t & align .~ MiddleRight
    colorTextList = hcat . map (uncurry colorText) -- TODO: Fix this

    entityText = fromMaybe "None" $ entry^.content
    entityName = textline entityFont entityText & align .~ MiddleLeft
    entityFont = makeFsAC 12 $ if
        | entry^.isFocused           -> Style.textFocusColor
        | isNothing (entry^.content) -> Style.textSecondaryColor
        | otherwise -> case entry^.prefix of
            Nothing -> Style.textPrimaryColor
            Just pfx -> if Text.isPrefixOf (fromString pfx) hintText
                then Style.textPrimaryColor
                else Style.textSecondaryColor

    ft = makeFs 12 Color.darkgray

layout_description :: Description -> Layout
layout_description d = vrel lns
    & setDefaultPadding
    where
    lns = [(40 @@ px  , ins)] <> sts
       <> [(20 @@ px, mempty), ( 1 @@ fill, des) , (40 @@ px  , ktu)]
    ins = textline ft (d^.name) & align .~ TopCenter
    des = text fs (d^.ff#description) & align .~ TopLeft

    ktu = case d^.ff#action of
        Nothing            -> mempty
        Just (KeyInfo k i) -> container $ textStyled
            [ (fs, "Press"), (fp, k), (fs, "to " <> i <> ".") ]
            & align .~ BottomLeft

    ft = makeFsAC 12 Style.textPrimaryColor
    fp = makeFsAC 10 Style.textPrimaryColor
    fs = makeFsAC 10 Style.textSecondaryColor

    sts = case d^.ff#stats of
        Nothing -> []
        Just  s -> catMaybes
            [ displayStat Color.red    "Attack %+d"     $ s^.ff#attack
            , displayStat Color.red    "Defence %+d"    $ s^.ff#defence
            , displayStat Color.red    "Max Health %+d" $ s^.ff#maxHealth
         -- , displayStat Color.purple "Max Speed %f"   $ s^.ff#maxSpeed
         -- , displayStat Color.purple "Max Range %f"   $ s^.ff#attackRange
            ]

    displayStat col f a
        | ua == 0   = Nothing
        | otherwise = Just (30 @@ px, statTextline col $ fromString $ printf f ua)
        where ua = Unwrapped a
    statTextline col msg = textline (makeFs 10 col) msg & align .~ Center


layout_container :: Container -> Layout
layout_container c = vrel [ (30 @@ px, tit), (1 @@ fill, con) ]
    where
    titleText = c^.title <> c^.hint
    titleLabel = textline ft titleText & align .~ MiddleLeft
    tit = setDefaultPadding $ container titleLabel
    con = layout_selectEntries $ c^.content
    ft = makeFs 12 Color.darkgray

setDefaultPadding :: Layout -> Layout
setDefaultPadding = set (padding.each) (8 @@ px)
