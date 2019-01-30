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
import qualified Data.HashMap.Strict as HashMap
-- import Random.Utils (pureRandomSeed)
-- import qualified System.Random.MWC as MWC

import Types.MenuState
import Types.InputState
import Types.GameState
import Types.ResourceManager (ResourceMap)

data St = St
   { stResources  :: ResourceMap
   , stMenuState  :: MenuState
   , stInputState :: InputState
   , stGameState  :: GameState
   -- , stRandomSeed :: MWC.Seed
   }
makeFields ''St

defaultSt :: MonadIO m => m St
defaultSt = return $ St
    { stResources  = HashMap.empty
    , stInputState = def
    , stGameState  = def
    , stMenuState  = def
    -- , stRandomSeed = pureRandomSeed
    }


