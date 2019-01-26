module GUI.Inventory
    ( inventoryLayout
    , pickupBoxPanelLayout
    ) where

import Delude
import qualified Data.Text as Text
import qualified Data.Map as PrefixMap
import qualified Data.Vector as Vector
import Data.Vector (Vector)

import Engine hiding (slots)
-- import Engine.Layout.Render
import Engine.Layout.Types hiding (content)
-- import Engine.Graphics.Types (RenderAction)

import Types
import Types.Entity
import Types.InputState
import Types.GUI
import Focus

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

withPadding :: Layout -> Layout
withPadding = simpleBox $ def
    & padding.each .~ Style.basePadding
    & size.width    .~ (1 @@ fill)
    & size.height   .~ (1 @@ fill)

equipmentLayout :: St -> Entity -> Layout
equipmentLayout st e =
    withPadding $ simpleLineupV $ map equipEntryLayout $ equipmentList st e

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
containersLayout st = simpleBox d $ simpleLineupV
    [ withTitle "Containers:" $ layoutEmpty
    , withTitle "Items on the ground: " $ itemsOnGroundLayout st
    ]
    where
    d = def
        & size.width    .~ (1 @@ fill)
        & size.height   .~ (1 @@ fill)
        -- & border.left.width .~ 1
        -- & border.left.color .~ Style.baseBorderColor


-- descriptionsLayout :: Layout
-- descriptionsLayout = fillBox $ simpleText "Descriptions"

itemsOnGroundLayout :: St -> Layout
itemsOnGroundLayout st = withPadding $ case st^.inputState.selectState of
    Just ss -> selectLayout st ss
    Nothing -> pickupBoxLayout fillDesc Nothing $ map ("",) es
    where
    es = focusItemsInRange st

fillDesc :: BoxDesc
fillDesc = def
    & size.width       .~ 1 @@ fill
    & size.height      .~ 1 @@ fill

--------------------------------------------------------------------------------

selectLayout :: St -> SelectState -> Layout
selectLayout st s = case s^.selectKind of
    SelectPickup v -> selectPickupLayout st cpfx smap v
    where
    cpfx = s^.currentPrefix
    smap = s^.selectMap

selectPickupLayout
    :: St -> Seq Char -> SelectMap -> Vector EntityId -> Layout
selectPickupLayout st cpfx smap vi =
    pickupBoxLayout fillDesc (Just $ toList cpfx) $ fromSelectMap smap vei
    where
    vei = Vector.mapMaybe toEntityWithId vi
    toEntityWithId i =
        EntityWithId i <$> lookupEntityById i (st^.gameState.entities)

fromSelectMap :: SelectMap -> Vector a -> [(String, a)]
fromSelectMap smap v = map snd . sortOn fst . mapMaybe f $ PrefixMap.toList smap
    where
    f (p, i) = (i,) . (p,) <$> Vector.indexM v i

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

