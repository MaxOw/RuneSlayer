module Main where

import Delude

import Engine (Ignition (..))
import qualified Engine

import Controller (handleEvent)
import Model      (integrate)
import View       (renderView)
import Game       (initSt, endSt)
import Reload.Utils (reacquire)

import qualified Version
import Dev.AltLayout (layoutDev)

main :: IO ()
main = do
    putTextLn Version.infoFull
    win <- reacquire 0 $ Engine.initWindow "RuneSlayer" (400, 400)

    Engine.igniteEngine win $ Ignition
        { initializer      = initSt
        , eventHandler     = handleEvent
        , renderer         = renderView
        , integrator       = integrate
        , finalizer        = endSt
        }

