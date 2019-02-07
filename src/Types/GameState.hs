{-# Language TemplateHaskell #-}
module Types.GameState where

import Delude
import qualified Control.Monad.Trans.State.Lazy as Lazy
import Types.Entity.Common (EntityId)
import Types.Entity (EntityIndex)
import Types.EntityAction (DirectedEntityAction)
import EntityIndex (emptyIndex)

data GameState = GameState
   { gameState_entities   :: EntityIndex
   , gameState_actions    :: [DirectedEntityAction]
   , gameState_focusId    :: Maybe EntityId
   , gameState_gameScale  :: Double
   , gameState_menuScale  :: Double
   , gameState_frameCount :: Word32
   }
makeFieldsCustom ''GameState

type GameStateM = Lazy.StateT GameState IO

instance Default GameState where
    def = GameState
        { gameState_entities   = emptyIndex
        , gameState_actions    = []
        , gameState_focusId    = Nothing
        , gameState_gameScale  = 64
        , gameState_menuScale  = 1.0
        , gameState_frameCount = 0
        }

