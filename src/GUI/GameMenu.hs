module GUI.GameMenu where

import Delude
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Zipper as Zipper

import Engine
import qualified Engine.Layout.Alt as Alt

import Types
import Types.InputState
import Types.Entity.PassiveType (InteractionName(..))
import Types.Entity.Agent (PlayerStatus)
import InputState (getMode, isPanelVisible, getInputString, showActionKeySeqs)
import Skills.Runes (getRuneByName)
import Focus (focusEntity)
import GameState.Query
import Entity
import Types.Entity.Common (EntityStatus(..))
import GameState (getGameOverScreen)

import qualified Types.GUI as Layout
import GUI.Inventory
import GUI.Layout
import qualified Tutorial
import qualified Messages

--------------------------------------------------------------------------------

gameMenuLayout :: Game Alt.Layout
gameMenuLayout = Alt.composition . catMaybes <$> sequence
    [ Just <$> statusPanesLayout
    , overlayMenuLayout ]
    where
    overlayMenuLayout = getMode >>= \case
        StatusMode m -> statusMenuLayout m
        RunicMode    -> runicModeLayout
     -- SpaceMode    -> spaceMenuLayout
        _            -> return Nothing -- layoutEmpty

--------------------------------------------------------------------------------

statusPanesLayout :: Game Alt.Layout
statusPanesLayout = Alt.composition . catMaybes <$> sequence
 -- [ rIf GroundPreviewPanel    groundPreviewPanelLayout
    [ pure Nothing
    , rIf StatusPanel statusPanelLayout
    , Tutorial.display
    , Just <$> Messages.display
    , actionsMenu
    ]
    where
    rIf panel renderFunc =
        isPanelVisible panel >>= \case
            True  -> renderFunc
            False -> pure Nothing

gameOverScreenLayout :: Game (Maybe Alt.Layout)
gameOverScreenLayout = getGameOverScreen >>= \case
    Nothing -> return Nothing
    Just gs -> return $ Just $ layout_gameOverScreen gs

statusPanelLayout :: Game (Maybe Alt.Layout)
statusPanelLayout = withPlayerStatus $ \ps ->
    let hir = Set.member EntityStatus_HostilesInRange (ps^.status)
    in layout_statusPanel $ def
        & ff#hostilesInRange .~ hir
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

actionsMenu :: Game (Maybe Alt.Layout)
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
            { field_actionName = toName (e^.entity.oracleName) a
            , field_actionHint = toText h
            }

    toName Nothing  a = show a
    toName (Just n) a = unInteractionName a <> " " <> n

storyDialog :: Game (Maybe Alt.Layout)
storyDialog = do
    msd <- use $ userState.inputState.ff#storyDialog
    ksq <- showActionKeySeqs StoryDialogMode InputAction_NextPage
    case msd of
        Nothing -> return Nothing
        Just sd -> return $ Just $ layout_storyDialog $ def
            & title   .~ sd^.title
            & content .~ fromMaybe "" (Zipper.focus $ sd^.ff#dialogPages)
            & ff#nextPageKey .~ ksq

runicModeLayout :: Game (Maybe Alt.Layout)
runicModeLayout = do
    ans <- getInputString
    iom <- (RunicMode ==) <$> getMode
    withPlayerStatus $ \ps -> layout_runicMode $ makeDesc iom ps ans
    where
    makeDesc iom ps ans = def
        & ff#showQuery  .~ (isJust qt && iom)
        & ff#queryText  .~ fromMaybe "" (view (ff#query) <$> qt)
        & ff#answerText .~ ans
        where
        qt = flip getRuneByName (ps^.ff#runicLevel) =<< ps^.ff#selectedRune

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

statusMenuLayout :: StatusMenu -> Game (Maybe Alt.Layout)
statusMenuLayout m = case m of
    StatusMenu_Inventory   -> Just <$> inventoryLayout
    StatusMenu_StoryDialog -> storyDialog

