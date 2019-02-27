module GUI.GameMenu where

import Delude

import Engine
import Engine.Layout.Types

import Types
import Types.InputState
import InputState (getMode, isPanelVisible)

import GUI.Common
import GUI.Inventory
import qualified GUI.Style as Style

--------------------------------------------------------------------------------

gameMenuLayout :: Game Layout
gameMenuLayout = overlayLayouts <$> sequence
    [ statusPanesLayout
    , overlayMenuLayout ]
    where
    overlayMenuLayout = getMode >>= \case
        StatusMode m -> statusMenuLayout m
        SpaceMode    -> spaceMenuLayout
        _            -> return layoutEmpty

--------------------------------------------------------------------------------

statusPanesLayout :: Game Layout
statusPanesLayout = isPanelVisible GroundPreviewPanel >>= \case
    True  -> groundPreviewPanelLayout
    False -> return layoutEmpty

--------------------------------------------------------------------------------

spaceMenuLayout :: Game Layout
spaceMenuLayout = return $ layoutBox desc []
    where
    desc = def
        & boxAlign         .~ BottomCenter
        & size.width       .~ 1.0 @@ wpct
        & size.height      .~ 0.4 @@ wpct
        & border.top.width .~ Style.baseBorderWidth
        & border.top.color .~ Style.baseBorderColor
        & padding.each     .~ Style.basePadding
        & color            .~ Style.baseBackgroundColor

--------------------------------------------------------------------------------

statusMenuLayout :: StatusMenu -> Game Layout
statusMenuLayout m = case m of
    StatusMenu_Inventory -> inventoryLayout
    -- _ -> return emptyLayout

