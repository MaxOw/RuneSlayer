{-# Language TemplateHaskell #-}
module Types.St
    ( St(..)
    , defaultSt

    , resources
    , inputState
    , gameState
    , menuState
    , randomSeed
    ) where

import Delude
-- import Random.Utils (pureRandomSeed)
-- import qualified System.Random.MWC as MWC

import Types.MenuState
import Types.InputState
import Types.GameState

type Resources  = ()
data St = St
   { stResources  :: Resources
   , stMenuState  :: MenuState
   , stInputState :: InputState
   , stGameState  :: GameState
   -- , stRandomSeed :: MWC.Seed
   }
makeFields ''St

defaultSt :: MonadIO m => m St
defaultSt = return $ St
    { stResources  = def
    , stInputState = def
    , stGameState  = def
    , stMenuState  = def
    -- , stRandomSeed = pureRandomSeed
    }


