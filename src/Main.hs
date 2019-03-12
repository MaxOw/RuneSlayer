module Main where

import Delude

import Engine (Ignition (..))
import qualified Engine

import Controller (handleEvent)
import Model      (integrate)
import View       (renderView)
import Game       (initSt, endSt)
import Reload.Utils (reacquire)

main :: IO ()
main = do
    win <- reacquire 0 $ Engine.initWindow "RuneSlayer" (400, 400)

    Engine.igniteEngine win $ Ignition
        { initializer      = initSt
        , eventHandler     = handleEvent
        , renderer         = renderView
        , integrator       = integrate
        , finalizer        = endSt
        }

