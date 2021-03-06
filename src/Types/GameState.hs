{-# Language DefaultSignatures #-}
module Types.GameState where

import Delude
import Types.Entity.Common (EntityId, Duration)
import Types.Entity (EntityIndex)
import Types.DirectedAction (DirectedAction)
import Types.Tutorial (TutorialState)
import Types.Story (StoryState)
import Types.Messages (SystemMessages)
import Types.Runes (RunesState)
import Types.MapEditor (MapEditorState)

data GameOverScreen = GameOverScreen
   { field_timer       :: Duration
   , field_pressAnyKey :: Bool
   } deriving (Generic)
instance Default GameOverScreen

data GameState = GameState
   { field_entities       :: EntityIndex
   , field_actions        :: [DirectedAction]
   , field_targetId       :: Maybe EntityId
   , field_gameScale      :: Float
   , field_frameCount     :: Word32
   , field_gameOverScreen :: Maybe GameOverScreen
   , field_tutorialState  :: TutorialState
   , field_storyState     :: StoryState
   , field_systemMessages :: SystemMessages
   , field_runesState     :: RunesState
   , field_mapEditorState :: MapEditorState
   } deriving (Generic)

type GameStateM = StateT GameState IO

class HasGameState s where
    gameState :: Lens' s GameState
    default gameState :: HasF "gameState" s GameState => Lens' s GameState
    gameState = ff#gameState

