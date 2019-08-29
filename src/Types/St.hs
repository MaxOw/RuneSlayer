module Types.St
    ( St(..)

    , GameWire (..)
    , Game
    ) where

import Delude

import Engine (Engine, EngineState, RenderAction, userState)
import Engine.Graphics.Scroller.Types (Scroller)

import Types.Config (Config)
import Types.MenuState
import Types.InputState
import Types.GameState
import Types.ResourceManager (Resources)
import Types.Debug (DebugFlag)

data St = St
   { field_resources  :: Resources
   , field_menuState  :: MenuState
   , field_inputState :: InputState
   , field_gameState  :: GameState
   , field_scroller   :: Scroller
   , field_debugFlags :: Set DebugFlag
   , field_overview   :: RenderAction
   , field_config     :: Config
   , field_wires      :: [GameWire]
   } deriving (Generic)
instance HasResources St Resources
instance HasGameState St
instance HasGameState (EngineState St) where gameState = userState.gameState

type Game a = Engine St a
newtype GameWire = GameWire { stepGameWire :: Game (Maybe GameWire) }

