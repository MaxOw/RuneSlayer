module Main where

import Delude
import Control.Exception (try, AsyncException)

import Engine (Ignition (..), igniteEngine)
import qualified Engine

import Controller (handleEvent)
import Model      (integrate)
import View       (renderView)
import Game       (initSt, endSt)
import Reload.Utils (reacquire)

-- import qualified Graphics.UI.GLFW as GLFW

import WorldGen (worldGenTest)

main :: IO ()
main = do
    worldGenTest
    win <- reacquire 0 $ Engine.initWindow "RuneSlayer" (400, 400)
    -- win <- Engine.initWindow "RuneSlayer" (400, 400)

    -- st  <- initSt

    eex <- try $ Engine.igniteEngine win $ Ignition
        { initializer      = initSt
        , eventHandler     = handleEvent
        , renderer         = renderView
        , integrator       = integrate
        , finalizer        = endSt
        }
    print (eex :: Either AsyncException ())

