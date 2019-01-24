module MenuState
    ( module Types.MenuState

    , getMenuState
    ) where

import Delude
import Engine (userState)
import Types (Game)
import Types.St
import Types.MenuState

getMenuState :: Game MenuState
getMenuState = use (userState.menuState)
