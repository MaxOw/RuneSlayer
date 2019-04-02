module Types.GameState where

import Delude
import qualified Control.Monad.Trans.State.Lazy as Lazy
import Types.Entity.Common (EntityId)
import Types.Entity (EntityIndex)
import Types.DirectedAction (DirectedAction)
import qualified EntityIndex

data GameState = GameState
   { field_entities       :: EntityIndex
   , field_actions        :: [DirectedAction]
   , field_focusId        :: Maybe EntityId
   , field_gameScale      :: Float
   , field_menuScale      :: Float
   , field_frameCount     :: Word32
   , field_changeCache    :: HashMap String Int
   } deriving (Generic)


type GameStateM = Lazy.StateT GameState IO

defaultGameState :: MonadIO m => m GameState
defaultGameState = do
    eix <- EntityIndex.new
    return $ GameState
        { field_entities       = eix
        , field_actions        = []
        , field_focusId        = Nothing
        , field_gameScale      = 64
        , field_menuScale      = 1.0
        , field_frameCount     = 0
        , field_changeCache    = mempty
        }

