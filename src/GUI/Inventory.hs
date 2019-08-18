module GUI.Inventory
    ( inventoryLayout
    -- , groundPreviewPanelLayout
    ) where

import Delude
import qualified Data.Text as Text
import qualified Data.Map as Map

import Types (Game, St)
import Types.InputState
import Engine (userState)
import Engine.Layout.Alt hiding (left)

import GameState.Query
import Focus
import Entity
import qualified Equipment
import Types.Equipment
import GUI.Layout (layout_inventory)
import Types.GUI as Layout

--------------------------------------------------------------------------------

inventoryLayout :: Game Layout
inventoryLayout = do
    eq <- getEquipmentList
    fi <- getFocusedItem
    cs <- getContainers
    st <- use userState
    return $ layout_inventory $ def
        & ff#equipment   .~ map (toSelectEntry st) eq
        & ff#description .~ fmap toDescription fi
        & ff#containers  .~ cs

--------------------------------------------------------------------------------

getContainers :: Game [Container]
getContainers = sequence
    [ backpackContainer
    , inspectedContainer
    , itemsOnGround
    ]

getTargetHint :: ItemMoveTarget -> Game Text
getTargetHint mt = maybe "" toText . flip hintForMoveTarget mt <$> use userState

makeContainer :: Text -> ItemMoveTarget -> [EntityId] -> Game Container
makeContainer tit tg is = do
    th <- getTargetHint tg
    es <- catMaybes <$> mapM lookupEntity is
    st <- use userState
    let ct = map (toSelectEntrySimple st) es
    return $ def
        & title   .~ tit
        & hint    .~ th
        & content .~ ct

backpackContainer :: Game Container
backpackContainer = focusEquipmentSlot EquipmentSlot_Backpack >>= \case
    Nothing -> return def
    Just e  -> do
        let tit = "Content of the "<> showEntityName (e^.entity) <> ": "
        let cs = e^..entity.oracleContent.traverse.traverse
        makeContainer tit ItemMoveTarget_Backpack cs

inspectedContainer :: Game Container
inspectedContainer = do
    st <- use userState
    fmap (fromMaybe def) $ runMaybeT $ do
        si <- MaybeT $ pure $ st^.inputState.inventoryState.ff#container
        et <- MaybeT $ lookupEntity si
        cs <- MaybeT $ pure $ et^.entity.oracleContent
        let tit = (fromMaybe "Container" $ et^.entity.oracleName) <> ": "
        MaybeT $ Just <$> makeContainer tit ItemMoveTarget_Container cs

itemsOnGround :: Game Container
itemsOnGround = do
    let tit = "Items on the ground: "
    es <- map (view entityId) <$> focusItemsInRange
    makeContainer tit ItemMoveTarget_Ground es

toSelectEntrySimple :: St -> EntityWithId -> Layout.SelectEntry
toSelectEntrySimple st x = toSelectEntry st (Nothing, Just x)

toSelectEntry
    :: St
    -> (Maybe EquipmentSlot, Maybe EntityWithId)
    -> Layout.SelectEntry
toSelectEntry st (mslot, meid) = def
    & label     .~ slotName
    & prefix    .~ mpfx
    & hint      .~ mhint
    & isFocused .~ fc
    & content   .~ fmap showEntityName meid
    where
    slotName = Text.stripPrefix "EquipmentSlot_" . show =<< mslot
    mpfx     = fmap toList $ st^?inputState.selectState.traverse.currentPrefix
    mhint    = hintForEntityIdOrSlot st (view entityId <$> meid) mslot
    fc       = fromMaybe False $ isItemFocused st <$> meid

toDescription :: EntityWithId -> Description
toDescription e = def
    & name .~ showEntityName e

getEquipmentList :: Game [(Maybe EquipmentSlot, Maybe EntityWithId)]
getEquipmentList = focusEntity >>= \case
    Nothing -> return []
    Just  e -> do
        let sls = e^..oracleEquipment.traverse.slots.folded
        let meq = e^.oracleEquipment
        let lookupSlot s = Equipment.lookupSlot s =<< meq
        forM sls $ \s -> do
            let me = lookupSlot s
            mewid <- join <$> traverse lookupEntity me
            return (Just s, mewid)

getFocusedItem :: Game (Maybe EntityWithId)
getFocusedItem = do
    st <- use userState
    let fi = st^.inputState.inventoryState.focusedItem
    join <$> traverse lookupEntity fi

showEntityName :: HasEntity e Entity => e -> Text
showEntityName (view entity -> e) = fromMaybe "???" (e^.oracleName)

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

isItemFocused :: HasEntityId e EntityId => St -> e -> Bool
isItemFocused st e = nothingFalse mfi (e^.entityId ==)
    where mfi = st^.inputState.inventoryState.focusedItem

--------------------------------------------------------------------------------

{-
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

-}

