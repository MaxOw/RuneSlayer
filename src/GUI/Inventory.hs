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
import Types.InputState
import GameState.Query
import Entity
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
    mh = hintForEntityIdOrSlot st (view entityId <$> me) (Just s)
    fc = nothingFalse me $ isItemFocused st

isItemFocused :: HasEntityId e EntityId => St -> e -> Bool
isItemFocused st e = nothingFalse mfi (e^.entityId ==)
    where mfi = st^.inputState.inventoryState.focusedItem

hintForEntityIdOrSlot
    :: St -> Maybe EntityId -> Maybe EquipmentSlot -> Maybe String
hintForEntityIdOrSlot st mei mes
    = viaNonEmpty head $ catMaybes
    [ hintForEntityId st =<< mei
    , hintForMoveTarget st . ItemMoveTarget_EquipmentSlot =<< mes
    ]

hintForEntityId :: HasEntityId e EntityId => St -> e -> Maybe String
hintForEntityId st e = case st^.inputState.selectState of
    Nothing -> Nothing
    Just ss -> case ss^.selectKind of
        SelectKind_Pickup v -> Map.lookup (e^.entityId) (v^.hintMap)
        SelectKind_Drop   v -> Map.lookup (e^.entityId) (v^.hintMap)
        SelectKind_Focus  v -> Map.lookup (e^.entityId) (v^.hintMap)
        SelectKind_MoveTo _ -> Nothing
        SelectKind_Action _ -> Nothing

hintForMoveTarget :: St -> ItemMoveTarget -> Maybe String
hintForMoveTarget st mt = case st^.inputState.selectState of
    Nothing -> Nothing
    Just ss -> case ss^.selectKind of
        SelectKind_Pickup _ -> Nothing
        SelectKind_Drop   _ -> Nothing
        SelectKind_Focus  _ -> Nothing
        SelectKind_MoveTo v -> Map.lookup mt (v^.hintMap)
        SelectKind_Action _ -> Nothing

equipmentList :: Game [(EquipmentSlot, Maybe EntityWithId)]
equipmentList = focusEntity >>= \case
    Nothing -> return []
    Just  e -> do
        let sls = e^..oracleEquipment.traverse.slots.folded
        let meq = e^.oracleEquipment
        let lookupSlot s = Equipment.lookupSlot s =<< meq
        forM sls $ \s -> do
            let me = lookupSlot s
            mewid <- join <$> traverse lookupEntity me
            return (s, mewid)

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
containersLayout = simpleBox def . simpleLineupV . catMaybes <$> sequence
    [ Just <$> backpackContainerLayout
    , inspectedContainerLayout
    , Just <$> itemsOnGroundLayout
    ]

backpackContainerLayout :: Game Layout
backpackContainerLayout = focusEquipmentSlot EquipmentSlot_Backpack >>= \case
    Nothing -> return layoutEmpty
    Just e  -> do
        th <- getTargetHint ItemMoveTarget_Backpack
        let tit = "Content of the "<> showEntityName (e^.entity) <> ": " <> th
        let cs = e^..entity.oracleContent.traverse.traverse
        st <- use userState
        es <- catMaybes <$> mapM lookupEntity cs
        return $ withTitle tit $ withPadding $ selectEntitiesLayout st es

getTargetHint :: ItemMoveTarget -> Game Text
getTargetHint mt = maybe "" toText . flip hintForMoveTarget mt <$> use userState

inspectedContainerLayout :: Game (Maybe Layout)
inspectedContainerLayout = do
    st <- use userState
    th <- getTargetHint ItemMoveTarget_Container
    runMaybeT $ do
        si <- MaybeT $ pure $ st^.inputState.inventoryState.ff#container
        et <- MaybeT $ lookupEntity si
        cs <- MaybeT $ pure $ et^.entity.oracleContent
        let t = (fromMaybe "Container" $ et^.entity.oracleName) <> ": " <> th
        MaybeT $ Just . containerLayout st t . catMaybes <$> mapM lookupEntity cs
    where
    containerLayout st t cs
        = withTitle t
        $ withPadding
        $ selectEntitiesLayout st cs

itemsOnGroundLayout :: Game Layout
itemsOnGroundLayout = do
    st <- use userState
    es <- focusItemsInRange
    th <- getTargetHint ItemMoveTarget_Ground
    return
        $ withTitle ("Items on the ground: " <> th)
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
showEntityName e = fromMaybe "???" (e^.oracleName)

