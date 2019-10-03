module Messages
    ( SystemMessages
    , update
    , addInfo, addHitEffect
    , displayInfo, displayLocated
    ) where

import Delude
import Engine (EngineState)
import Engine.Layout.Alt (Layout)
import Types.Entity.Common (Location, Health, defaultDelta)
import Types (Game, St)
import Types.GameState (gameState)

import Types.Messages
import Layout.Messages

--------------------------------------------------------------------------------

-- | Add info system message to display.
addInfo :: Text -> Game ()
addInfo msg = do
    dur <- use $ systemMessages.ff#maxDisplayDuration
    let smsg = SystemMessage msg Nothing MessageKind_Info dur dur
    systemMessages.messages %= (smsg :)

-- | Add hit effect system message to display.
addHitEffect :: Location -> Health -> Game ()
addHitEffect loc hp = do
    let dur = 1
    let msg = show (- Unwrapped hp)
    let smsg = SystemMessage msg (Just loc) MessageKind_HitEffect dur dur
    systemMessages.messages %= (smsg :)

-- | Update system messages queue.
update :: Game ()
update = do
    systemMessages.messages %= updateMessages
    where
    updateMessages
        = filter (\x -> x^.duration > 0)
        . map (over duration $ subtract defaultDelta)

-- | Display info system messages window layout.
displayInfo :: Game Layout
displayInfo = do
    maxCount <- use $ systemMessages.ff#maxMessagesCount
    msgs <- use $ systemMessages.messages
    let ims = filter (\x -> x^.ff#messageKind == MessageKind_Info) msgs
    return $ layout_infoMessages $ take maxCount ims

displayLocated :: (Location -> V2 Float) -> Game Layout
displayLocated conv = do
    msgs <- use $ systemMessages.messages
    return $ layout_locatedMessages conv msgs

--------------------------------------------------------------------------------

-- Helper lens
systemMessages :: Lens' (EngineState St) SystemMessages
systemMessages = gameState.ff#systemMessages

