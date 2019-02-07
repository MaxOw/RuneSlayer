module Main where

import Delude

import Engine (Ignition (..), igniteEngine)
import qualified Engine

import Controller (handleEvent)
import Model      (integrate)
import View       (renderView)
import Game       (initSt)
import Reload.Utils (reacquire)

import qualified Graphics.UI.GLFW as GLFW

-- import qualified Data.Map as PrefixMap

main :: IO ()
main = do
    win <- reacquire 0 $ Engine.initWindow "RuneSlayer" (400, 400)
    GLFW.makeContextCurrent $ Just win
    -- win <- Engine.initWindow "RuneSlayer" (400, 400)

    -- st  <- initSt

    Engine.igniteEngine win $ Ignition
        { initializer      = initSt
        , eventHandler     = handleEvent
        , renderer         = renderView
        , integrator       = integrate
        }

{-
test :: IO ()
test = do
    let m = PrefixMap.fromList [ ("aa", 1), ("ab", 2), ("c", 3) ]
    print m
    print $ PrefixMap.splitLookup "a" m
    print $ PrefixMap.splitLookup "b" m
    print $ PrefixMap.splitLookup "c" m
    print $ PrefixMap.takeWhileAntitone (isPrefixOf "a") m
    print $ PrefixMap.takeWhileAntitone (isPrefixOf "c") m
    -}

