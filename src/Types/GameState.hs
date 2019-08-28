module Types.GameState where

import Delude
import Types.Entity.Common (EntityId, Duration)
import Types.Entity (EntityIndex)
import Types.DirectedAction (DirectedAction)
import Types.Tutorial (TutorialState)

data GameOverScreen = GameOverScreen
   { field_timer       :: Duration
   , field_pressAnyKey :: Bool
   } deriving (Generic)
instance Default GameOverScreen

data GameState = GameState
   { field_entities       :: EntityIndex
   , field_actions        :: [DirectedAction]
   , field_focusId        :: Maybe EntityId
   , field_gameScale      :: Float
   , field_menuScale      :: Float
   , field_frameCount     :: Word32
   , field_changeCache    :: HashMap String Int
   , field_gameOverScreen :: Maybe GameOverScreen
   , field_tutorialState  :: TutorialState
   } deriving (Generic)

type GameStateM = StateT GameState IO
