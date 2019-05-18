module Types.St
    ( St(..)
    , defaultSt

    ) where

import Delude

import Engine (RenderAction)
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
   } deriving (Generic)
instance HasResources St Resources

defaultSt :: MonadIO m => EntityIndex -> Scroller -> m St
defaultSt eix scro = do
    gs <- defaultGameState eix
    return $ St
        { field_resources  = def
        , field_inputState = def
        , field_gameState  = gs
        , field_menuState  = def
        , field_scroller   = scro
        , field_debugFlags = def
        , field_overview   = mempty
        , field_config     = def
        }

