module Types.St
    ( St(..)
    , defaultSt

    , GameWire (..)
    , Game
    ) where

import Delude

import Engine (Engine, RenderAction)
import Engine.Graphics.Scroller.Types (Scroller)

import Types.Config (Config)
import Types.Entity (EntityIndex)
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

type Game a = Engine St a
newtype GameWire = GameWire { stepGameWire :: Game (Maybe GameWire) }

defaultSt :: MonadIO m => EntityIndex -> Scroller -> m St
defaultSt eix scro = do
    gs <- defaultGameState eix
    return $ St
        { field_resources  = def
        , field_inputState = defaultInputState
        , field_gameState  = gs
        , field_menuState  = def
        , field_scroller   = scro
        , field_debugFlags = def
        , field_overview   = mempty
        , field_config     = def
        , field_wires      = def
        }

