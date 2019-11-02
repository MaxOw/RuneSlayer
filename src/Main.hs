module Main where

import Delude

import Engine (Ignition (..))
import qualified Engine

import Controller (handleEvent)
import Model      (integrate)
import View       (renderView)
import Game       (initSt, setupSt, endSt)

import qualified Version

main :: IO ()
main = do
    putTextLn Version.infoFull
    win <- Engine.initWindow "RuneSlayer" (1400, 1400) False

    Engine.igniteEngine win $ Ignition
        { initializer  = initSt
        , stateSetup   = setupSt
        , eventHandler = handleEvent
        , renderer     = renderView
        , integrator   = integrate
        , finalizer    = endSt
        }
