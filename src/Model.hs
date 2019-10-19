module Model
    ( integrate
    ) where

import Delude
import Engine (userState)

import Types (Game, Integrator, stepGameWire)
import Types.EntityAction

import GameState
import InputState

--------------------------------------------------------------------------------

integrate :: Integrator
integrate _time = do -- getMenuState >>= \case
    updateWires
    updateGameState
    preformActiveActions
    zoomGameState $ frameCount += 1

--------------------------------------------------------------------------------

updateWires :: Game ()
updateWires
    = assign (userState.ff#wires)
    =<< fmap catMaybes . mapM stepGameWire
    =<< use (userState.ff#wires)

preformActiveActions :: Game ()
preformActiveActions = do
    moveVector <- makeMoveVector
    whenChanged_ moveVector $ actOnFocusedEntity . EntityAction_SetMoveVector

makeMoveVector :: Game (V2 Float)
makeMoveVector = V2
    <$> makeActiveMove MoveRight MoveLeft
    <*> makeActiveMove MoveUp    MoveDown

makeActiveMove :: MoveDirection -> MoveDirection -> Game Float
makeActiveMove d0 d1 = do
    let f x = if x then 1 else 0
    m0 <- isActionActive (ActiveMove d0)
    m1 <- isActionActive (ActiveMove d1)
    return $ f m0 - f m1

-- updateRandomGen :: Game ()
-- updateRandomGen = userState.randomGen %= snd . Random.split
