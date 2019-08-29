module Messages
    ( SystemMessages
    , add, update, display
    ) where

import Delude
import Engine (EngineState)
import Engine.Layout.Alt (Layout)
import Types.Entity.Common (defaultDelta)
import Types (Game, St)
import Types.GameState (gameState)

import Types.Messages
import Layout.Messages

--------------------------------------------------------------------------------

-- | Add system message to display.
add :: Text -> Game ()
add msg = do
    maxDuration <- use $ systemMessages.ff#maxDisplayDuration
    systemMessages.messages %= (SystemMessage msg maxDuration :)

-- | Update system messages queue.
update :: Game ()
update = do
    maxCount <- use $ systemMessages.ff#maxMessagesCount
    systemMessages.messages %= take maxCount . updateMessages
    where
    updateMessages
        = filter (\x -> x^.duration > 0)
        . map (over duration $ subtract defaultDelta)

-- | Display system messages window layout.
display :: Game Layout
display = do
    nms <- use $ systemMessages.messages
    return $ layout_systemMessages nms

--------------------------------------------------------------------------------

-- Helper lens
systemMessages :: Lens' (EngineState St) SystemMessages
systemMessages = gameState.ff#systemMessages

