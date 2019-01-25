module GUI.GameMenu where

import Delude
import qualified Data.Map as PrefixMap
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified Data.Text as Text

-- import qualified Engine
import Engine
import Engine.Layout.Render
import Engine.Layout.Types
import Engine.Graphics.Types (RenderAction)

import Types
import Types.Entity
import Types.Entity.Common (EntityId)
import Types.InputState
import InputState (isPanelVisible)
import GameState (entityIdToWithId)
import Focus

import GUI.Common
import GUI.Inventory
import qualified GUI.Style as Style

--------------------------------------------------------------------------------

renderGameMenu :: St -> Entity -> Graphics RenderAction
renderGameMenu st e = do
    sps <- renderStatusPanes st e
    gmn <- gameMenu
    return $ renderComposition [ sps, gmn ]
    -- return $ renderComposition [ gmn ]
    where
    gameMenu = case st^.inputState.mode of
        StatusMode m -> renderStatusMenu m st e
        SpaceMode    -> renderSpaceMenu st
        _ -> return $ Engine.renderComposition []

renderStatusPanes :: St -> Entity -> Graphics RenderAction
renderStatusPanes st _e = do
    let es = focusItemsInRange st
    case st^.inputState.selectState of
        Just ss -> normalSelectRender ss
        Nothing -> do
            vp <- isPanelVisible GroundPreviewPanel
            if vp
            then pickupBoxRender Nothing $ map ("",) es
            else return mempty
    -- where
    {-
    displayNamesList = simpleLineupV . map showName
    showName = simpleBox boxDesc . simpleText
             . fromMaybe "???" . view (entity.oracle.name)
    boxDesc = def & size.height .~ (30 @@ px)
    -}

normalSelectRender :: SelectState -> Graphics RenderAction
normalSelectRender s = case s^.selectKind of
    SelectPickup v -> normalSelectPickupRender cpfx smap v
    where
    cpfx = s^.currentPrefix
    smap = s^.selectMap

normalSelectPickupRender
    :: Seq Char -> SelectMap -> Vector EntityId -> Graphics RenderAction
normalSelectPickupRender cpfx smap vi = do
    vei <- Vector.mapMaybe id <$> mapM entityIdToWithId vi
    pickupBoxRender (Just $ toList cpfx) $ fromSelectMap smap vei

pickupBoxRender
    :: Maybe String -> [(String, EntityWithId)] -> Graphics RenderAction
pickupBoxRender mpfx es
    = makeRenderLayout $ simpleBox desc $ simpleLineupV
    $ map (selectFieldLayout mpfx) es
    where
    desc = Style.baseBox
        & boxAlign         .~ TopRight
        & size.width       .~ 300 @@ px
        & size.height      .~ 200 @@ px

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

fromSelectMap :: SelectMap -> Vector a -> [(String, a)]
fromSelectMap smap v = map snd . sortOn fst . mapMaybe f $ PrefixMap.toList smap
    where
    f (p, i) = (i,) . (p,) <$> Vector.indexM v i

renderSpaceMenu :: St -> Graphics RenderAction
renderSpaceMenu _st = do
    rend <- makeRenderLayout $ Layout_Box desc []
    return rend
    where
    desc = def
        & boxAlign         .~ BottomCenter
        & size.width       .~ 1.0 @@ wpct
        & size.height      .~ 0.4 @@ wpct
        & border.top.width .~ Style.baseBorderWidth
        & border.top.color .~ Style.baseBorderColor
        & padding.each     .~ Style.basePadding
        & color            .~ Style.baseBackgroundColor

renderStatusMenu :: StatusMenu -> St -> Entity -> Graphics RenderAction
renderStatusMenu m st e = case m of
    StatusMenu_Inventory -> renderInventory st e
    -- _ -> return mempty

