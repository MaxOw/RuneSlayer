module GUI.Inventory
    ( inventoryLayout
    , groundPreviewPanelLayout
    ) where

import Delude
import qualified Data.Text as Text
import qualified Data.Map as Map

import Engine hiding (slots)
import Engine.Layout.Types

import Types
import Types.Entity
import Types.InputState
import Focus

import Types.Equipment
import qualified GUI.Style as Style
import GUI.Common
import qualified Equipment

--------------------------------------------------------------------------------

data SelectFieldEntry = SelectFieldEntry
   { field_prefix    :: Maybe String
   , field_label     :: Maybe Text
   , field_hint      :: Maybe String
   , field_isFocused :: Bool
   , field_content   :: Maybe EntityWithId
   } deriving (Generic)

instance Default SelectFieldEntry

--------------------------------------------------------------------------------

inventoryLayout :: Game Layout
inventoryLayout = menuBox opts . simpleLineupH <$> sequence
    [ inventoryLeftLayout
    , containersLayout ]
    where
    opts = def
         & title .~ "Inventory"
         & size .~ Size (0.8 @@ wpct) (0.8 @@ wpct)

inventoryLeftLayout :: Game Layout
inventoryLeftLayout = simpleBox def . simpleLineupV <$> sequence
    [ equipmentLayout
    , selectedItemDescriptionLayout
    ]

--------------------------------------------------------------------------------

equipmentLayout :: Game Layout
equipmentLayout = do
    st <- use userState
    withPadding . simpleLineupV . map (equipEntryLayout st) <$> equipmentList

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

equipmentList :: Game [(EquipmentSlot, Maybe EntityWithId)]
equipmentList = focusEntity >>= \case
    Nothing -> return []
    Just  e -> do
        let sls = e^..oracle.equipment.traverse.slots.folded
        let meq = e^.oracle.equipment
        let lookupSlot s = Equipment.lookupSlot s =<< meq
        forM sls $ \s -> do
            let me = lookupSlot s
            mewid <- join <$> traverse lookupEntity me
            return (s, mewid)
        -- zip sls $ map (lookupEntity st <=< lookupSlot) sls

    -- lookupSlot :: EquipmentSlot -> Maybe EntityId

--------------------------------------------------------------------------------

selectedItemDescriptionLayout :: Game Layout
selectedItemDescriptionLayout = do
    st <- use userState
    let fi = st^.inputState.inventoryState.focusedItem
    sit <- join <$> traverse lookupEntity fi
    case sit of
        Nothing -> return layoutEmpty
        Just e  -> itemDescriptionLayout e

itemDescriptionLayout :: HasEntity e Entity => e -> Game Layout
itemDescriptionLayout (view entity -> e)
    = return $ simpleBox d $ withTitle "Selected item description:"
    $ withPadding $ simpleText $ showEntityName e
    where
    d = def
        & border.top.width .~ Style.baseBorderWidth
        & border.top.color .~ Style.baseBorderColor

--------------------------------------------------------------------------------

containersLayout :: Game Layout
containersLayout = simpleBox def . simpleLineupV <$> sequence
    [ backpackContainerLayout
    , itemsOnGroundLayout
    ]

backpackContainerLayout :: Game Layout
backpackContainerLayout = focusEquipmentSlot EquipmentSlot_Backpack >>= \case
    Nothing -> return layoutEmpty
    Just e  -> do
        let tit = "Content of the "<> showEntityName (e^.entity) <>":"
        let cs = e^..entity.oracle.content.traverse.traverse
        st <- use userState
        es <- catMaybes <$> mapM lookupEntity cs
        return $ withTitle tit $ withPadding $ selectEntitiesLayout st es

itemsOnGroundLayout :: Game Layout
itemsOnGroundLayout = do
    st <- use userState
    es <- focusItemsInRange
    return
        $ withTitle "Items on the ground: "
        $ withPadding
        $ selectEntitiesLayout st es

selectEntitiesLayout :: St -> [EntityWithId] -> Layout
selectEntitiesLayout st = simpleLineupV . map f
    where
    mpfx = fmap toList $ st^?inputState.selectState.traverse.currentPrefix
    f e = selectFieldEntryLayout $ def
        & prefix    .~ mpfx
        & hint      .~ hintForEntityId st e
        & isFocused .~ isItemFocused st e
        & content   .~ Just e

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

--------------------------------------------------------------------------------

groundPreviewPanelLayout :: Game Layout
groundPreviewPanelLayout = do
    st <- use userState
    es <- focusItemsInRange
    return
        $ simpleBox desc
        $ selectEntitiesLayout st es
    where
    desc = Style.baseBox
        & boxAlign         .~ TopRight
        & size.width       .~ 300 @@ px
        & size.height      .~ 200 @@ px

--------------------------------------------------------------------------------

showEntityName :: Entity -> Text
showEntityName e = fromMaybe "???" (e^.oracle.name)

