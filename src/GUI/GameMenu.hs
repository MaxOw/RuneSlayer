module GUI.GameMenu where

import Delude
import qualified Data.Set as Set

import Engine
import Engine.Layout.Types
import qualified Engine.Layout.Alt as Alt

import Types
import Types.InputState
import Types.Entity.Player (PlayerStatus)
import InputState (getMode, isPanelVisible, getInputString)
import Skills.Runes (RunicSlots, listRunicSlots, getRuneByName)
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
    , rIf DefensiveSlotsPanel   defensiveSlotsPanelLayout
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
offensiveSlotsPanelLayout = slotsPanelLayout
    (OffensiveMode ==)
    (view $ ff#offensiveSlots)
    layout_offensiveSlotsPanel

defensiveSlotsPanelLayout :: Game Alt.Layout
defensiveSlotsPanelLayout = slotsPanelLayout
    (DefensiveMode ==)
    (view $ ff#defensiveSlots)
    layout_defensiveSlotsPanel

slotsPanelLayout
    :: (InputMode -> Bool)
    -> (PlayerStatus -> RunicSlots)
    -> (SlotsPanelDesc -> Alt.Layout)
    -> Game Alt.Layout
slotsPanelLayout cndMode viewSlots layoutF = do
    ans <- getInputString
    iom <- fmap cndMode getMode
    mfo <- focusEntity
    case flip entityOracle EntityQuery_PlayerStatus =<< mfo of
        Nothing -> return def
        Just ps -> return $ layoutF $ makeDesc iom ps ans
    where
    makeDesc iom ps ans = def
        & ff#slots      .~ ss
        & ff#showQuery  .~ (isJust qt && iom)
        & ff#queryText  .~ fromMaybe "" (view (ff#query) <$> qt)
        & ff#answerText .~ ans
        where
        ss = map SlotDesc $ listRunicSlots $ viewSlots ps
        qt = flip getRuneByName (ps^.ff#runicLevel) =<< ps^.ff#selectedRune

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

