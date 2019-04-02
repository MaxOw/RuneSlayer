module Types.St
    ( St(..)
    , defaultSt

    ) where

import Delude

import Engine.Graphics.Scroller.Types (Scroller)

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
   } deriving (Generic)
instance HasResources St Resources


defaultSt :: MonadIO m => Scroller -> m St
defaultSt scro = do
    gs <- defaultGameState
    return $ St
        { field_resources  = def
        , field_inputState = def
        , field_gameState  = gs
        , field_menuState  = def
        , field_scroller   = scro
        , field_debugFlags = def
        }


