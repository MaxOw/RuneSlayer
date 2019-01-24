module Model
    ( integrate
    ) where

import Delude
-- import qualified Engine
-- import Engine (userState)

import Types (Game, Integrator)
import Types.EntityAction

import GameState
-- import MenuState
import InputState
-- import Types.St (randomSeed)

--------------------------------------------------------------------------------

integrate :: Integrator
integrate _time = do -- getMenuState >>= \case
    updateGameState
    preformActiveActions
    zoomGameState $ frameCount += 1

preformActiveActions :: Game ()
preformActiveActions = do
    moveVector <- makeMoveVector
    actOnFocusedEntity (EntityAction_SetMoveVector moveVector)

makeMoveVector :: Game (V2 Double)
makeMoveVector = V2
    <$> makeActiveMove MoveRight MoveLeft
    <*> makeActiveMove MoveUp    MoveDown

makeActiveMove :: MoveDirection -> MoveDirection -> Game Double
makeActiveMove d0 d1 = do
    let f x = if x then 1 else 0
    m0 <- isActionActive (ActiveMove d0)
    m1 <- isActionActive (ActiveMove d1)
    return $ f m0 - f m1

-- updateRandomGen :: Game ()
-- updateRandomGen = userState.randomGen %= snd . Random.split
