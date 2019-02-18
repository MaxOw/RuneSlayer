{-# Language TemplateHaskell #-}
module Types.St
    ( St(..)
    , defaultSt

    , resources
    , inputState
    , gameState
    , menuState
    , randomSeed
    , scroller
    ) where

import Delude
import qualified Data.HashMap.Strict as HashMap
-- import Random.Utils (pureRandomSeed)
-- import qualified System.Random.MWC as MWC

import Engine.Graphics.Scroller.Types (Scroller)

import Types.MenuState
import Types.InputState
import Types.GameState
import Types.ResourceManager (ResourceMap)
import Types.Debug (DebugFlag)

data St = St
   { st_resources  :: ResourceMap
   , st_menuState  :: MenuState
   , st_inputState :: InputState
   , st_gameState  :: GameState
   , st_scroller   :: Scroller
   , st_debugFlags :: Set DebugFlag
   -- , st_randomSeed :: MWC.Seed
   }
makeFieldsCustom ''St

defaultSt :: MonadIO m => Scroller -> m St
defaultSt scro = return $ St
    { st_resources  = HashMap.empty
    , st_inputState = def
    , st_gameState  = def
    , st_menuState  = def
    , st_scroller   = scro
    , st_debugFlags = def
    -- , st_randomSeed = pureRandomSeed
    }


