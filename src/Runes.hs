module Runes
    ( init
    , getMasteryList
    , inputHandler
    , display
    , startRunicMode
    , clearLastResult
    , maxMasteryLevel
    ) where

import Delude hiding (init)
import Data.Time.Clock (getCurrentTime)
import Engine (EngineState, Key(..))
import Engine.Layout.Alt (Layout)
import Types (Game, St)
import Types.EntityAction (PlayerAction (..))
import Types.GameState (gameState)
import Types.InputState
import Types.Runes
import Types.Skills.Runes
import Skills.Runes
    (isCorrectAnswer, updateRuneMastery, maxMasteryLevel, masteryToProbWeight)
import Random.Utils

import Data.Generics.Product.Subtype (smash, upcast)
import qualified Data.IxSet.Typed as IxSet

import GameState (actOnPlayer)
import InputState.Actions
import InputKeymap (keyToChar)
import GUI.Layout (layout_runicMode)

--------------------------------------------------------------------------------

init :: MonadIO m => RuneSet -> m RunesState
init rs = do
    return $ RunesState
        { field_allRunes       = rs
        , field_mastery        = addToMasterySet rs def
        , field_currentRune    = def
        , field_lastRuneName   = def
        , field_lastRuneResult = def
        , field_level          = 2
        }

-- addKnownRunes :: RuneSet -> Game ()
-- addKnownRunes rs = runesState.mastery %= addToMasterySet rs

getMasteryList :: Game [RuneMastery]
getMasteryList = uses (runesState.mastery) $ IxSet.toAscList (Proxy @MasteryLevel)

addToMasterySet :: RuneSet -> MasterySet -> MasterySet
addToMasterySet = IxSet.insertList . map uprune . toList
    where
    uprune :: Rune -> RuneMastery
    uprune x = smash x def

inputHandler :: Keypress -> Game ()
inputHandler kp = case keypressKey kp of
    k | Just ch <- keyToChar k -> appendInputString ch -- >> autoAccept
    Key'Backspace -> backspaceInputString
    Key'Enter     -> acceptAnswer
    Key'Space     -> acceptAnswer
    _             -> return ()
    where
    -- autoAccept = whenM verifyAnswer acceptAnswer

acceptAnswer :: Game ()
acceptAnswer = getInputString >>= \ans -> when (ans /= "") $ do
    ver <- verifyAnswer ans
    updateUsage ver ans
    clearInputString
    currentRune .= Nothing
    selectNextRune
    -- inputActionEscape

verifyAnswer :: Text -> Game Bool
verifyAnswer ans = use currentRune >>= \case
    Nothing -> return False
    Just cr -> return $ isCorrectAnswer ans cr

updateUsage :: Bool -> Text -> Game ()
updateUsage ver ans = whenJustM (use currentRune) $ \r -> do
    p <- uses (runesState.ff#level._Wrapped) RunicPoints
    actOnPlayer $ PlayerAction_UpdateRune p ver
    t <- liftIO getCurrentTime
    let u = RuneUsage t ver
    runesState.mastery %= updateRuneMastery (r^.name) u
    runesState.lastRuneName   .= Just (r^.name)
    runesState.lastRuneResult .= Just (RuneResult r ans ver)

startRunicMode :: Game ()
startRunicMode = do
    selectNextRune
    setMode RunicMode

selectNextRune :: Game ()
selectNextRune = whenNothingM_ (use currentRune) $ do
    rs <- use runesState
    let lu = rs^.lastRuneName
    let ft = maybe id (\r -> filter (\x -> x^.name /= r)) lu
    let ls = ft $ levRunesList (rs^.mastery)
    fr <- runRandomIO $ uniformSelectWeighted $ map toProb ls
    currentRune .= (upcast <$> fr)
    where
    levRunesList = IxSet.toAscList (Proxy @MasteryLevel)
    toProb x = (masteryToProbWeight $ x^.ff#masteryLevel, x)

display :: Game Layout
display = do
    ans <- getInputString
    mcr <- use currentRune
    mlr <- use $ runesState.lastRuneResult
    return $ layout_runicMode $ def
        & ff#queryText  .~ fromMaybe "" (mcr^?traverse.ff#query)
        & ff#answerText .~ ans
        & ff#prevResult .~ mlr

clearLastResult :: Game ()
clearLastResult = runesState.lastRuneResult .= Nothing

--------------------------------------------------------------------------------

{-
type SaveId = ()

load :: SaveId -> Game ()
load _ = return ()

save :: SaveId -> Game ()
save _ = return ()
-}

--------------------------------------------------------------------------------

runesState :: Lens' (EngineState St) RunesState
runesState = gameState.ff#runesState

currentRune :: Lens' (EngineState St) (Maybe Rune)
currentRune = runesState.ff#currentRune
