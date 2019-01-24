module GUI.Inventory
    ( renderInventory
    ) where

import Delude
import qualified Data.Text as Text

import Engine hiding (slots)
import Engine.Layout.Render
import Engine.Layout.Types hiding (content)
import Engine.Graphics.Types (RenderAction)

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

renderInventory :: St -> Entity -> Graphics RenderAction
renderInventory st e = makeRenderLayout $ menuBox opts $ simpleLineupH
    [ equipmentLayout st e, containersLayout ] --, descriptionsLayout ]
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

containersLayout :: Layout
containersLayout = fillBox $ simpleText "Containers"

-- descriptionsLayout :: Layout
-- descriptionsLayout = fillBox $ simpleText "Descriptions"

