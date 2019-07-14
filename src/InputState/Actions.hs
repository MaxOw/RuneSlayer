module InputState.Actions where

import Delude

import Engine (userState)
import Types (Game)
import Types.Entity (EntityWithId)
import Types.Entity.Common (EntityId)
import Types.InputState
import GameState.Query

--------------------------------------------------------------------------------

zoomInputState :: InputStateM a -> Game a
zoomInputState = zoom (userState.inputState)

--------------------------------------------------------------------------------

getMode :: Game InputMode
getMode = use (userState.inputState.mode)

setMode :: InputMode -> Game ()
setMode m = assign (userState.inputState.mode) m

--------------------------------------------------------------------------------

-- Show contents of given entity in a inventory window.
inspectContent :: EntityId -> Game ()
inspectContent eid = do
    setMode $ StatusMode Inventory
    zoomInputState $ inventoryState.ff#container .= Just eid

getInventoryContainer :: Game (Maybe EntityWithId)
getInventoryContainer = do
    mi <- zoomInputState $ use $ inventoryState.ff#container
    case mi of
        Nothing -> return Nothing
        Just ic -> lookupEntity ic

--------------------------------------------------------------------------------

clearInventoryState :: Game ()
clearInventoryState = zoomInputState $ inventoryState .= def

