module GUI.GameMenu where

import Delude
import qualified Data.Set as Set
import qualified Data.Map as Map

import Engine
import Engine.Layout.Alt (Layout)
import qualified Engine.Layout.Alt as Layout

import Types
import Types.Entity.Common (Location)
import Types.InputState
import Types.Entity.PassiveType (InteractionName(..))
import Types.Entity.Agent (PlayerStatus)
import InputState (getMode, isPanelVisible, showActionKeySeqs)
import Focus
import GameState.Query
import Entity
import Entity.HasField
import Types.Entity.Common (EntityStatus(..))
import GameState (getGameOverScreen)

import qualified Types.GUI as Layout
import GUI.Inventory
import GUI.Layout
import qualified Tutorial
import qualified Messages
import qualified Runes
import qualified Story

--------------------------------------------------------------------------------

gameMenuLayout :: Game Layout
gameMenuLayout = Layout.composition <$> sequence
    [ statusPanesLayout
    , overlayMenuLayout ]
    where
    overlayMenuLayout = getMode >>= \case
        InventoryMode   -> inventoryLayout
        StoryDialogMode -> Story.display
        RunicStatusMode -> runicStatusLayout
        RunicMode    -> Runes.display
     -- SpaceMode    -> spaceMenuLayout
        _            -> return mempty


--------------------------------------------------------------------------------

overlayLayout :: (Location -> V2 Float) -> Game Layout
overlayLayout conv = Layout.composition . catMaybes <$> sequence
    [ Just <$> Messages.displayLocated conv
    , interactionOverlay conv
    , attackOverlay conv
    ]

interactionOverlay :: (Location -> V2 Float) -> Game (Maybe Layout)
interactionOverlay conv = runMaybeT $ do
    (e, _) <- MaybeT focusNearestInteractionInRange
    n      <- MaybeT $ pure $ e^.entity.oraclePrimaryInteraction
    MaybeT $ overlayLabelFor conv NormalMode Interact e (Unwrapped n)

attackOverlay :: (Location -> V2 Float) -> Game (Maybe Layout)
attackOverlay conv = runMaybeT $ do
    f <- MaybeT $ focusEntity
    s <- MaybeT $ pure $ f^.oraclePlayerStatus
    t <- MaybeT $ pure $ s^.target
    e <- MaybeT $ lookupEntity t
    if s^.canAttackTarget
    then MaybeT $ overlayLabelFor conv NormalMode ExecuteAttack e "Attack"
    else MaybeT $ return Nothing

overlayLabelFor
    :: HasEntity e Entity
    => (Location -> V2 Float)
    -> InputMode -> InputAction -> e -> Text
    -> Game (Maybe Layout)
overlayLabelFor conv m act (view entity -> e) lab = do
    rightMode <- (m ==) <$> getMode
    case e^.oracleLocation of
        Just loc | rightMode -> do
            kact <- showActionKeySeqs m act
            let off = fromMaybe 0 $ e^.oracleLabelOffset
            let pos = conv $ over _Wrapped (+off) loc
            return $ Just $ layout_actionableLabel kact lab pos
        _ -> return Nothing

moveOverhead :: Location -> Location
moveOverhead = over (_Wrapped._y) (+unitHeight)
    where
    unitHeight = 1.7

statusPanesLayout :: Game Layout
statusPanesLayout = Layout.composition . catMaybes <$> sequence
 -- [ rIf GroundPreviewPanel    groundPreviewPanelLayout
    [ pure Nothing
    , rIf StatusPanel statusPanelLayout
    , Tutorial.display
    , Just <$> Messages.displayInfo
    , actionsMenu
    ]
    where
    rIf panel renderFunc =
        isPanelVisible panel >>= \case
            True  -> renderFunc
            False -> pure Nothing

gameOverScreenLayout :: Game (Maybe Layout)
gameOverScreenLayout = getGameOverScreen >>= \case
    Nothing -> return Nothing
    Just gs -> return $ Just $ layout_gameOverScreen gs

statusPanelLayout :: Game (Maybe Layout)
statusPanelLayout = do
    ir <- not . null <$> focusItemsInRange
    withPlayerStatus $ \ps ->
        let hr = Set.member EntityStatus_HostilesInRange (ps^.status)
        in layout_statusPanel $ def
            & ff#hostilesInRange .~ hr
            & ff#itemsInRange    .~ ir
            & ff#attackMode      .~ ps^.ff#attackMode
            & ff#health          .~ makeHealth ps
            & ff#runes           .~ makeRunes  ps
    where
    makeHealth s = def
        & ff#points    .~ s^.health._Wrapped
        & ff#maxPoints .~ s^.ff#fullStats.maxHealth._Wrapped
    makeRunes s = def
        & ff#points    .~ s^.ff#runicPoints._Wrapped
        & ff#maxPoints .~ s^.ff#maxRunicPoints._Wrapped

withPlayerStatus :: (PlayerStatus -> a) -> Game (Maybe a)
withPlayerStatus f = do
    mfo <- focusEntity
    case flip entityOracle EntityQuery_PlayerStatus =<< mfo of
        Nothing -> return Nothing
        Just ps -> return $ Just $ f ps

actionsMenu :: Game (Maybe Layout)
actionsMenu = do
    fmap layout_actionsMenu <$> getHintMap
    where
    getHintMap = do
        ss <- use $ userState.inputState.selectState
        case ss^?traverse.selectKind of
            Just (SelectKind_Action v) | not (Map.null $ v^.hintMap) ->
                Just . catMaybes <$> (mapM toDesc $ Map.assocs $ v^.hintMap)
            _ -> return Nothing

    toDesc :: ((EntityId, InteractionName), [Char]) -> Game (Maybe Layout.ActionHint)
    toDesc ((eid, a), h) = lookupEntity eid >>= \case
        Nothing -> return Nothing
        Just  e -> return $ Just $ Layout.ActionHint
            { field_actionName = toName (e^.entity.oracleDisplayName) a
            , field_actionHint = toText h
            }

    toName Nothing  a = show a
    toName (Just n) a = unInteractionName a <> " " <> n

runicStatusLayout :: Game Layout
runicStatusLayout = do
    ml <- Runes.getMasteryList
    return $ layout_runesStatus $ def
        & ff#runes .~ map conv ml
        & ff#maxMastery .~ Unwrapped Runes.maxMasteryLevel
    where
    conv x = Layout.RuneStatus
        (x^.ff#runeId)
        (x^.ff#answers)
        (x^.ff#masteryLevel._Wrapped)

--------------------------------------------------------------------------------

{-
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
-}

--------------------------------------------------------------------------------

