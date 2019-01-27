{-# Language TemplateHaskell #-}
module GUI.Inventory
    ( inventoryLayout
    , pickupBoxPanelLayout
    ) where

import Delude
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Map as PrefixMap
import qualified Data.Vector as Vector
import Data.Vector (Vector)

import Engine hiding (slots)
-- import Engine.Layout.Render
import Engine.Layout.Types hiding (HasContent, content)
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

--------------------------------------------------------------------------------

data SelectFieldEntry = SelectFieldEntry
   { selectFieldEntry_prefix  :: Maybe String
   , selectFieldEntry_label   :: Maybe Text
   , selectFieldEntry_hint    :: Maybe String
   , selectFieldEntry_content :: Maybe EntityWithId
   } deriving (Generic)
makeFieldsCustom ''SelectFieldEntry
instance Default SelectFieldEntry

--------------------------------------------------------------------------------

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
    withPadding $ simpleLineupV $ map (equipEntryLayout st) $ equipmentList st e

equipEntryLayout :: St -> (EquipmentSlot, Maybe EntityWithId) -> Layout
equipEntryLayout st (s, me) = selectFieldEntryLayout $ def
    & label   .~ Just lb
    & prefix  .~ mpfx
    & hint    .~ mh
    & content .~ me
    where
    lb = Text.drop (length ("EquipmentList_" :: String)) $ show s
    mpfx = fmap toList $ st^?inputState.selectState.traverse.currentPrefix
    mh = hintForEntityId st . view entityId =<< me

hintForEntityId :: St -> EntityId -> Maybe String
hintForEntityId st eid = case st^.inputState.selectState of
    Nothing -> Nothing
    Just ss -> case ss^.selectKind of
        SelectDrop v -> Map.lookup eid (v^.hintMap)
        _            -> Nothing

equipmentList :: St -> Entity -> [(EquipmentSlot, Maybe EntityWithId)]
equipmentList st e = zip sls $ map (lookupEntity st <=< lookupSlot) sls
    where
    sls = e^..oracle.equipment.traverse.slots.folded
    meq = e^.oracle.equipment

    lookupSlot :: EquipmentSlot -> Maybe EntityId
    lookupSlot s = Equipment.lookupSlot s =<< meq

containersLayout :: St -> Layout
containersLayout st = simpleBox d $ simpleLineupV
    [ backpackContainerLayout st
    , withTitle "Items on the ground: " $ itemsOnGroundLayout st
    ]
    where
    d = def
        & size.width    .~ (1 @@ fill)
        & size.height   .~ (1 @@ fill)
        -- & border.left.width .~ 1
        -- & border.left.color .~ Style.baseBorderColor

showEntityName :: Entity -> Text
showEntityName e = fromMaybe "???" (e^.oracle.name)

backpackContainerLayout :: St -> Layout
backpackContainerLayout st = case mei of
    Nothing -> layoutEmpty
    Just ei -> withTitle tit
        $ pickupBoxLayout fillDesc Nothing $ map ("",) es
        where
        tit = "Content of the "<> showEntityName (ei^.entity) <>":"
        -- cs :: [EntityWithId]
        es = lookupEntities st $ ei^..entity.oracle.content.traverse.traverse

    where
    mei = focusEquipmentSlot st EquipmentSlot_Backpack

itemsOnGroundLayout :: St -> Layout
itemsOnGroundLayout st = withPadding $ case st^.inputState.selectState of
    Just ss -> selectLayout st ss
    Nothing -> itemsInRangeLayout st

fillDesc :: BoxDesc
fillDesc = def
    & size.width       .~ 1 @@ fill
    & size.height      .~ 1 @@ fill

--------------------------------------------------------------------------------

itemsInRangeLayout :: St -> Layout
itemsInRangeLayout st = pickupBoxLayout fillDesc Nothing $ map ("",) es
    where
    es = focusItemsInRange st

selectLayout :: St -> SelectState -> Layout
selectLayout st s = case s^.selectKind of
    SelectPickup v -> selectPickupLayout st cpfx smap $ view values v
    SelectDrop   _ -> itemsInRangeLayout st
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
selectFieldLayout mpfx (p, e) = selectFieldEntryLayout $ def
    & prefix  .~ mpfx
    & hint    .~ Just p
    & content .~ Just e

selectFieldEntryLayout :: SelectFieldEntry -> Layout
selectFieldEntryLayout f = simpleBox boxDesc $ simpleLineupH
    $ addLabel [ prefixLayout , entityLayout ]
    where
    prefixLayout = simpleBox prefixDesc prefixText
    entityLayout = colorText entityColor $ case f^.content of
        Nothing -> "None"
        Just  e -> fromMaybe "???" (e^.entity.oracle.name)

    addLabel = case f^.label of
        Nothing -> id
        Just lb -> ((simpleText lb):)

    textHint = fromString $ fromMaybe "" $ f^.hint
    prefixText = case f^.prefix of
        Nothing -> colorText Style.textPrimaryColor textHint
        Just pfx -> if Text.isPrefixOf (fromString pfx) textHint
            then prefixTextHighlight pfx
            else colorText Style.textSecondaryColor textHint
    prefixTextHighlight pfx = colorTextList
        [(Style.textHintHighlightColor, textPfx)
        ,(Style.textHintColor         , textRest)]
        where
        textPfx = fromString pfx
        textRest = fromMaybe "" $ Text.stripPrefix textPfx textHint

    entityColor
        | isNothing (f^.content) = Style.textSecondaryColor
        | otherwise = case f^.prefix of
            Nothing -> Style.textPrimaryColor
            Just pfx -> if Text.isPrefixOf (fromString pfx) textHint
                then Style.textPrimaryColor
                else Style.textSecondaryColor

    prefixDesc = def
        & size.width .~ (30 @@ px)
        & padding.left .~ 8
    boxDesc = def & size.height .~ (30 @@ px)

