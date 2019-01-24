module GUI.GameMenu where

import Delude

-- import qualified Engine
import Engine
import Engine.Layout.Render
import Engine.Layout.Types
import Engine.Graphics.Types (RenderAction)

import Types
import Types.Entity
import Types.InputState
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
    makeRenderLayout $ Layout_Box desc [ displayNamesList es ]
    where
    desc = Style.baseBox
        & boxAlign         .~ TopRight
        & size.width       .~ 300 @@ px
        & size.height      .~ 200 @@ px

    displayNamesList = simpleLineupV . map showName
    showName = simpleBox boxDesc . simpleText
             . fromMaybe "???" . view (entity.oracle.name)
    boxDesc = def & size.height .~ (30 @@ px)

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

