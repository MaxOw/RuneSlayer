module GUI.Inventory
    ( inventoryLayout
    , pickupBoxPanelLayout
    ) where

import Delude
import qualified Data.Text as Text

import Engine hiding (slots)
-- import Engine.Layout.Render
import Engine.Layout.Types hiding (content)
-- import Engine.Graphics.Types (RenderAction)

import Types
import Types.Entity
import Types.GUI

import Types.Entity.Common
import Types.Equipment
import qualified GUI.Style as Style
import GUI.Common
import qualified Equipment

import EntityIndex (lookupEntityById)

-- import qualified Data.Colour as Color
-- import qualified Data.Colour.Names as Color

inventoryLayout :: St -> Entity -> Layout
inventoryLayout st e = menuBox opts $ simpleLineupH
    [ equipmentLayout st e, containersLayout st ] --, descriptionsLayout ]
    where
    opts = def
         & title .~ "Inventory"
         & size .~ Size (0.8 @@ wpct) (0.8 @@ wpct)

equipmentLayout :: St -> Entity -> Layout
equipmentLayout st e =
    fillBox $ simpleLineupV $ map equipEntryLayout $ equipmentList st e

equipEntryLayout :: (EquipmentSlot, Maybe Entity) -> Layout
equipEntryLayout (s, me) = simpleBox boxDesc $ simpleLineupH
    [ simpleText showName
    , colorText col showEntity
    ]
    where
    showName = toName $ show s
    toName = Text.drop (length ("EquipmentList_" :: String))
    showEntity = case me of
        Nothing -> "None"
        Just  e -> fromMaybe "???" (e^.oracle.name)

    col = if isJust me
        then Style.textPrimaryColor
        else Style.textSecondaryColor

    boxDesc = def
        & size.height .~ (30 @@ px)

equipmentList :: St -> Entity -> [(EquipmentSlot, Maybe Entity)]
equipmentList st e = zip sls $ map (getEntity <=< lookupSlot) sls
    where
    sls = e^..oracle.equipment.traverse.slots.folded
    meq = e^.oracle.equipment

    lookupSlot :: EquipmentSlot -> Maybe EntityId
    lookupSlot s = Equipment.lookup s =<< meq

    getEntity :: EntityId -> Maybe Entity
    getEntity eid = lookupEntityById eid (st^.gameState.entities)

containersLayout :: St -> Layout
containersLayout st = fillBox $ simpleLineupV
    [ fillBox $ simpleText "Containers:"
    , fillBox $ simpleText "Items on the ground:"
    ]

-- descriptionsLayout :: Layout
-- descriptionsLayout = fillBox $ simpleText "Descriptions"

--------------------------------------------------------------------------------

pickupBoxPanelLayout
    :: Maybe String -> [(String, EntityWithId)] -> Layout
pickupBoxPanelLayout = pickupBoxLayout desc
    where
    desc = Style.baseBox
        & boxAlign         .~ TopRight
        & size.width       .~ 300 @@ px
        & size.height      .~ 200 @@ px

pickupBoxLayout
    :: BoxDesc -> Maybe String -> [(String, EntityWithId)] -> Layout
pickupBoxLayout desc mpfx
    = simpleBox desc . simpleLineupV . map (selectFieldLayout mpfx)

selectFieldLayout :: Maybe String -> (String, EntityWithId) -> Layout
selectFieldLayout mpfx (p, e) = simpleBox boxDesc $ simpleLineupH
    [ prefixLayout , entityLayout ]
    where
    prefixLayout = simpleBox prefixDesc prefixText
    entityLayout = colorText entityColor $ fromMaybe "???" (e^.entity.oracle.name)

    textHint = fromString p
    prefixText = case mpfx of
        Nothing -> colorText Style.textPrimaryColor textHint
        Just pfx -> if isPrefixOf pfx p
            then prefixTextHighlight pfx
            else colorText Style.textSecondaryColor textHint
    prefixTextHighlight pfx = colorTextList
        [(Style.textHintHighlightColor, textPfx)
        ,(Style.textHintColor         , textRest)]
        where
        textPfx = fromString pfx
        textRest = fromMaybe "" $ Text.stripPrefix textPfx textHint

    entityColor = case mpfx of
        Nothing -> Style.textPrimaryColor
        Just pfx -> if isPrefixOf pfx p
            then Style.textPrimaryColor
            else Style.textSecondaryColor

    prefixDesc = def
        & size.width .~ (30 @@ px)
        & padding.left .~ 8
    boxDesc = def & size.height .~ (30 @@ px)

