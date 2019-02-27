module Model
    ( integrate
    ) where

import Delude
import Data.Hashable (hash)
import qualified Data.HashMap.Strict as HashMap
import Engine (userState)
import Types.St (gameState)

import Types (Game, Integrator)
import Types.EntityAction

import GameState
import InputState

--------------------------------------------------------------------------------

integrate :: Integrator
integrate _time = do -- getMenuState >>= \case
    updateGameState
    preformActiveActions
    zoomGameState $ frameCount += 1

--------------------------------------------------------------------------------

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

whenChanged_ :: (HasCallStack, Hashable a) => a -> (a -> Game ()) -> Game ()
whenChanged_ newValue doAction = do
    let cname = prettyCallStack callStack
    let newHash = hash newValue
    cmap <- use $ userState.gameState.changeCache
    case HashMap.lookup cname cmap of
        Nothing      -> valueChanged cname newHash
        Just oldHash -> if oldHash /= newHash
            then valueChanged cname newHash
            else return ()
    where
    valueChanged cname newHash = do
        -- putStrLn cname
        userState.gameState.changeCache %= HashMap.insert cname newHash
        doAction newValue

-- updateRandomGen :: Game ()
-- updateRandomGen = userState.randomGen %= snd . Random.split
