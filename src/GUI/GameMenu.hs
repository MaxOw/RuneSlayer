module GUI.GameMenu where

import Delude
-- import qualified Data.Map as PrefixMap
-- import qualified Data.Vector as Vector
-- import Data.Vector (Vector)
-- import qualified Data.Text as Text

-- import qualified Engine
import Engine
import Engine.Layout.Types

import Types
import Types.Entity
-- import Types.Entity.Common (EntityId)
import Types.InputState
import InputState (isPanelVisible)
-- import GameState (entityIdToWithId)
-- import EntityIndex (lookupEntityById)
import Focus

import GUI.Common
import GUI.Inventory
import qualified GUI.Style as Style

--------------------------------------------------------------------------------

gameMenuLayout :: St -> Entity -> Layout
gameMenuLayout st e = overlayLayouts
    [ statusPanesLayout st e
    , overlayMenuLayout ]
    where
    overlayMenuLayout = case st^.inputState.mode of
        StatusMode m -> statusMenuLayout m st e
        SpaceMode    -> spaceMenuLayout st
        _ -> layoutEmpty

--------------------------------------------------------------------------------

statusPanesLayout :: St -> Entity -> Layout
statusPanesLayout st _e = do
    let es = focusItemsInRange st
    if isPanelVisible GroundPreviewPanel st
    then pickupBoxPanelLayout Nothing $ map ("",) es
    else layoutEmpty

--------------------------------------------------------------------------------

spaceMenuLayout :: St -> Layout
spaceMenuLayout _st = layoutBox desc []
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

statusMenuLayout :: StatusMenu -> St -> Entity -> Layout
statusMenuLayout m st e = case m of
    StatusMenu_Inventory -> inventoryLayout st e
    -- _ -> return emptyLayout

