module InputState.Actions where

import Delude
import qualified Data.Zipper as Zipper
import qualified Data.Text as Text

import Engine (userState)
import Types (Game)
import Types.Entity (EntityWithId)
import Types.Entity.Script (StoryDialog)
import Types.Entity.Common (EntityId)
import Types.InputState
import GameState.Query
import qualified InputKeymap

--------------------------------------------------------------------------------

zoomInputState :: InputStateM a -> Game a
zoomInputState = zoom (userState.inputState)

--------------------------------------------------------------------------------

getMode :: Game InputMode
getMode = use (userState.inputState.mode)

setMode :: InputMode -> Game ()
setMode = assign (userState.inputState.mode)

--------------------------------------------------------------------------------

-- Show contents of given entity in a inventory window.
inspectContent :: EntityId -> Game ()
inspectContent eid = do
    setMode InventoryMode
    zoomInputState $ inventoryState.ff#container .= Just eid

getInventoryContainer :: Game (Maybe EntityWithId)
getInventoryContainer = do
    mi <- zoomInputState $ use $ inventoryState.ff#container
    case mi of
        Nothing -> return Nothing
        Just ic -> lookupEntity ic

--------------------------------------------------------------------------------

showStoryDialog :: EntityId -> StoryDialog -> Game ()
showStoryDialog eid sd = do
    setMode StoryDialogMode
    zoomInputState $ ff#storyDialog .= Just sds
    where
    sds = StoryDialogState
        { field_title       = sd^.title
        , field_entityId    = eid
        , field_dialogPages = Zipper.fromList $ sd^.ff#dialogPages
        }

showActionKeySeqs :: InputMode -> InputAction -> Game Text
showActionKeySeqs m a = uses (userState.inputState.inputKeymap)
    $ fromString . InputKeymap.showKeySeqs . InputKeymap.lookupInputAction a m

--------------------------------------------------------------------------------

clearInventoryState :: Game ()
clearInventoryState = zoomInputState $ inventoryState .= def

--------------------------------------------------------------------------------

inputActionEscape :: Game ()
inputActionEscape = zoomInputState $ do
    hist .= empty
    mode %= escapeMode
    selectState .= Nothing
    where
    escapeMode = \case
        _ -> NormalMode

--------------------------------------------------------------------------------

getInputString :: Game Text
getInputString = uses (userState.inputState.ff#inputString)
    (Text.strip . fromList @Text . toList)

appendInputString :: Char -> Game ()
appendInputString x = userState.inputState.ff#inputString %= (|> x)

backspaceInputString :: Game ()
backspaceInputString = userState.inputState.ff#inputString %= dropR1

clearInputString :: Game ()
clearInputString = userState.inputState.ff#inputString .= mempty

dropR1 :: Snoc (t x) (t x) x x => t x -> t x
dropR1 (l:>_) = l
dropR1 x = x

