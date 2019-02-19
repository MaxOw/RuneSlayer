{-# Language TemplateHaskell #-}
module Types.GameState where

import Delude
import qualified Control.Monad.Trans.State.Lazy as Lazy
import Types.Entity.Common (EntityId)
import Types.Entity (EntityIndexIO, EntityIndex)
import Types.EntityAction (DirectedEntityAction)
import EntityIndex (newIndex, unsafeFreezeEntityIndex)

data GameState = GameState
   { gameState_entitiesIO     :: EntityIndexIO
   , gameState_entities       :: EntityIndex
   -- ^ Frozen version of EntityIndexIO that should only by used for queries and
   -- should not be saved anywhere. Any assumptions of referential transparency
   -- should be thrown out of the window when using it. It should be assumed
   -- that it's only valid and unchanged for a period of a given frame.
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
    eix <- newIndex
    fx  <- unsafeFreezeEntityIndex eix
    return $ GameState
        { gameState_entitiesIO     = eix
        , gameState_entities       = fx
        , gameState_actions        = []
        , gameState_focusId        = Nothing
        , gameState_gameScale      = 64
        , gameState_menuScale      = 1.0
        , gameState_frameCount     = 0
        , gameState_changeCache    = mempty
        }

