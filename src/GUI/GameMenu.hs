module GUI.GameMenu where

import Delude
import qualified Data.Map as PrefixMap
import qualified Data.Vector as Vector
import Data.Vector (Vector)
-- import qualified Data.Text as Text

-- import qualified Engine
import Engine
import Engine.Layout.Types

import Types
import Types.Entity
import Types.Entity.Common (EntityId)
import Types.InputState
import InputState (isPanelVisible)
-- import GameState (entityIdToWithId)
import EntityIndex (lookupEntityById)
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
    case st^.inputState.selectState of
        Just ss -> normalSelectLayout st ss
        Nothing -> do
            if isPanelVisible GroundPreviewPanel st
            then pickupBoxPanelLayout Nothing $ map ("",) es
            else layoutEmpty

normalSelectLayout :: St -> SelectState -> Layout
normalSelectLayout st s = case s^.selectKind of
    SelectPickup v -> normalSelectPickupLayout st cpfx smap v
    where
    cpfx = s^.currentPrefix
    smap = s^.selectMap

normalSelectPickupLayout
    :: St -> Seq Char -> SelectMap -> Vector EntityId -> Layout
normalSelectPickupLayout st cpfx smap vi =
    pickupBoxPanelLayout (Just $ toList cpfx) $ fromSelectMap smap vei
    where
    vei = Vector.mapMaybe toEntityWithId vi
    toEntityWithId i =
        EntityWithId i <$> lookupEntityById i (st^.gameState.entities)

fromSelectMap :: SelectMap -> Vector a -> [(String, a)]
fromSelectMap smap v = map snd . sortOn fst . mapMaybe f $ PrefixMap.toList smap
    where
    f (p, i) = (i,) . (p,) <$> Vector.indexM v i


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

