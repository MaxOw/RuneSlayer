{-# Language TemplateHaskell #-}
module Types.GameState where

import Delude
import qualified Control.Monad.Trans.State.Lazy as Lazy
import Types.Entity.Common (EntityId)
import Types.Entity (EntityIndex)
import Types.EntityAction (DirectedEntityAction)
import qualified EntityIndex

data GameState = GameState
   { gameState_entities       :: EntityIndex
   , gameState_actions        :: [DirectedEntityAction]
   , gameState_focusId        :: Maybe EntityId
   , gameState_gameScale      :: Double
   , gameState_menuScale      :: Double
   , gameState_frameCount     :: Word32
   , gameState_changeCache    :: HashMap String Int
   }
makeFieldsCustom ''GameState

type GameStateM = Lazy.StateT GameState IO

defaultGameState :: MonadIO m => m GameState
defaultGameState = do
    eix <- EntityIndex.new
    return $ GameState
        { gameState_entities       = eix
        , gameState_actions        = []
        , gameState_focusId        = Nothing
        , gameState_gameScale      = 64
        , gameState_menuScale      = 1.0
        , gameState_frameCount     = 0
        , gameState_changeCache    = mempty
        }

