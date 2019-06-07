module Types.GameState where

import Delude
import Types.Entity.Common (EntityId)
import Types.Entity (EntityIndex)
import Types.DirectedAction (DirectedAction)

data GameState = GameState
   { field_entities       :: EntityIndex
   , field_actions        :: [DirectedAction]
   , field_focusId        :: Maybe EntityId
   , field_gameScale      :: Float
   , field_menuScale      :: Float
   , field_frameCount     :: Word32
   , field_changeCache    :: HashMap String Int
   } deriving (Generic)


type GameStateM = StateT GameState IO

defaultGameState :: MonadIO m => EntityIndex -> m GameState
defaultGameState eix = do
    return $ GameState
        { field_entities       = eix
        , field_actions        = []
        , field_focusId        = Nothing
        , field_gameScale      = 64
        , field_menuScale      = 1.0
        , field_frameCount     = 0
        , field_changeCache    = mempty
        }

