module GUI.GameMenu where

import Delude
import qualified Data.Set as Set

import Engine
import Engine.Layout.Types
import qualified Engine.Layout.Alt as Alt

import Types
import Types.InputState
import InputState (getMode, isPanelVisible, getInputString)
import Skills.Runes (listRunicSlots)
import Focus (focusEntity)
import Entity
import Types.Entity.Common (EntityStatus(..))

import Types.GUI
import GUI.Common
import GUI.Inventory
import GUI.Layout
import qualified GUI.Style as Style
-- import qualified Color

--------------------------------------------------------------------------------

gameMenuLayout :: Game Layout
gameMenuLayout = overlayLayouts <$> sequence
    -- [ statusPanesLayout
    [ overlayMenuLayout ]
    where
    overlayMenuLayout = getMode >>= \case
        StatusMode m -> statusMenuLayout m
        SpaceMode    -> spaceMenuLayout
        _            -> return layoutEmpty

--------------------------------------------------------------------------------

statusPanesLayout :: Game Alt.Layout
statusPanesLayout = Alt.composition . catMaybes <$> sequence
 -- [ rIf GroundPreviewPanel    groundPreviewPanelLayout
    [ pure Nothing
    , rIf StatusPanel           statusPanelLayout
    , rIf OffensiveSlotsPanel   offensiveSlotsPanelLayout
    ]
    where
    rIf panel renderFunc =
        isPanelVisible panel >>= \case
            True  -> Just <$> renderFunc
            False -> pure Nothing

statusPanelLayout :: Game Alt.Layout
statusPanelLayout = do
    mfo <- focusEntity
    case flip entityOracle EntityQuery_PlayerStatus =<< mfo of
        Nothing -> return def
        Just ps -> do
            let hir = Set.member EntityStatus_HostilesInRange (ps^.status)
            return $ layout_statusPanel $ def
                & ff#hostilesInRange .~ hir
                & ff#attackMode      .~ ps^.ff#attackMode

offensiveSlotsPanelLayout :: Game Alt.Layout
offensiveSlotsPanelLayout = do
    ans <- getInputString
    iom <- fmap (OffensiveMode ==) getMode
    mfo <- focusEntity
    case flip entityOracle EntityQuery_PlayerStatus =<< mfo of
        Nothing -> return def
        Just ps -> return $ layout_offensiveSlotsPanel $ makeDesc iom ps ans
    where
    makeDesc iom ps ans = def
        & ff#slots      .~ ss
        & ff#showQuery  .~ iom
        & ff#answerText .~ ans
        where
        ss = map SlotDesc $ listRunicSlots $ ps^.ff#offensiveSlots
        -- ss = map (SlotDesc . fromMaybe 0 . flip IntMap.lookup m) $ take n [0..]
        -- m = ps^.ff#offensiveSlots.ff#slots
        -- n = ps^.ff#offensiveSlots.ff#slotsCount

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

