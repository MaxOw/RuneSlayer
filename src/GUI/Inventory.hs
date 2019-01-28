{-# Language TemplateHaskell #-}
module GUI.Inventory
    ( inventoryLayout
    , groundPreviewPanelLayout
    ) where

import Delude
import qualified Data.Text as Text
import qualified Data.Map as Map

import Engine hiding (slots)
import Engine.Layout.Types hiding (HasContent, content)

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

--------------------------------------------------------------------------------

data SelectFieldEntry = SelectFieldEntry
   { selectFieldEntry_prefix    :: Maybe String
   , selectFieldEntry_label     :: Maybe Text
   , selectFieldEntry_hint      :: Maybe String
   , selectFieldEntry_isFocused :: Bool
   , selectFieldEntry_content   :: Maybe EntityWithId
   } deriving (Generic)
makeFieldsCustom ''SelectFieldEntry
instance Default SelectFieldEntry

--------------------------------------------------------------------------------

inventoryLayout :: St -> Entity -> Layout
inventoryLayout st e = menuBox opts $ simpleLineupH
    [ inventoryLeftLayout st e, containersLayout st ]
    where
    opts = def
         & title .~ "Inventory"
         & size .~ Size (0.8 @@ wpct) (0.8 @@ wpct)

inventoryLeftLayout :: St -> Entity -> Layout
inventoryLeftLayout st e = simpleBox def $ simpleLineupV
    [ equipmentLayout st e
    , selectedItemDescriptionLayout st
    ]

equipmentLayout :: St -> Entity -> Layout
equipmentLayout st e =
    withPadding $ simpleLineupV $ map (equipEntryLayout st) $ equipmentList st e

selectedItemDescriptionLayout :: St -> Layout
selectedItemDescriptionLayout st = case sit of
    Nothing -> layoutEmpty
    Just e  -> itemDescriptionLayout st e
    where
    sit = lookupEntity st =<< st^.inputState.inventoryState.focusedItem

itemDescriptionLayout :: HasEntity e Entity => St -> e -> Layout
itemDescriptionLayout st (view entity -> e)
    = simpleBox d $ withTitle "Selected item description:"
    $ withPadding $ simpleText $ showEntityName e
    where
    d = def
        & border.top.width .~ Style.baseBorderWidth
        & border.top.color .~ Style.baseBorderColor

equipEntryLayout :: St -> (EquipmentSlot, Maybe EntityWithId) -> Layout
equipEntryLayout st (s, me) = selectFieldEntryLayout $ def
    & label     .~ Just lb
    & prefix    .~ mpfx
    & hint      .~ mh
    & isFocused .~ fc
    & content   .~ me
    where
    lb = Text.drop (length ("EquipmentList_" :: String)) $ show s
    mpfx = fmap toList $ st^?inputState.selectState.traverse.currentPrefix
    mh = hintForEntityId st . view entityId =<< me
    fc = nothingFalse me $ isItemFocused st

isItemFocused :: HasEntityId e EntityId => St -> e -> Bool
isItemFocused st e = nothingFalse mfi (e^.entityId ==)
    where mfi = st^.inputState.inventoryState.focusedItem

hintForEntityId :: HasEntityId e EntityId => St -> e -> Maybe String
hintForEntityId st e = case st^.inputState.selectState of
    Nothing -> Nothing
    Just ss -> case ss^.selectKind of
        SelectPickup v -> Map.lookup (e^.entityId) (v^.hintMap)
        SelectDrop   v -> Map.lookup (e^.entityId) (v^.hintMap)
        SelectFocus  v -> Map.lookup (e^.entityId) (v^.hintMap)

equipmentList :: St -> Entity -> [(EquipmentSlot, Maybe EntityWithId)]
equipmentList st e = zip sls $ map (lookupEntity st <=< lookupSlot) sls
    where
    sls = e^..oracle.equipment.traverse.slots.folded
    meq = e^.oracle.equipment

    lookupSlot :: EquipmentSlot -> Maybe EntityId
    lookupSlot s = Equipment.lookupSlot s =<< meq

containersLayout :: St -> Layout
containersLayout st = simpleBox def $ simpleLineupV
    [ backpackContainerLayout st
    , itemsOnGroundLayout st
    ]

showEntityName :: Entity -> Text
showEntityName e = fromMaybe "???" (e^.oracle.name)

backpackContainerLayout :: St -> Layout
backpackContainerLayout st = case focusEquipmentSlot st EquipmentSlot_Backpack of
    Nothing -> layoutEmpty
    Just e  -> withTitle tit $ withPadding $ selectEntitiesLayout st es
        where
        tit = "Content of the "<> showEntityName (e^.entity) <>":"
        es = lookupEntities st $ e^..entity.oracle.content.traverse.traverse

itemsOnGroundLayout :: St -> Layout
itemsOnGroundLayout st
    = withTitle "Items on the ground: "
    $ withPadding
    $ selectEntitiesLayout st
    $ focusItemsInRange st

selectEntitiesLayout :: St -> [EntityWithId] -> Layout
selectEntitiesLayout st = simpleLineupV . map f
    where
    mpfx = fmap toList $ st^?inputState.selectState.traverse.currentPrefix
    f e = selectFieldEntryLayout $ def
        & prefix    .~ mpfx
        & hint      .~ hintForEntityId st e
        & isFocused .~ isItemFocused st e
        & content   .~ Just e

--------------------------------------------------------------------------------

groundPreviewPanelLayout :: St -> Layout
groundPreviewPanelLayout st
    = simpleBox desc
    $ selectEntitiesLayout st
    $ focusItemsInRange st
    where
    desc = Style.baseBox
        & boxAlign         .~ TopRight
        & size.width       .~ 300 @@ px
        & size.height      .~ 200 @@ px

selectFieldEntryLayout :: SelectFieldEntry -> Layout
selectFieldEntryLayout f = simpleBox boxDesc $ simpleLineupH
    $ addLabel [ prefixLayout , entityLayout ]
    where
    prefixLayout = simpleBox prefixDesc prefixText
    entityLayout = colorText entityColor $ case f^.content of
        Nothing -> "None"
        Just  e -> showEntityName (e^.entity)

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
        | f^.isFocused           = Style.textFocusColor
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

