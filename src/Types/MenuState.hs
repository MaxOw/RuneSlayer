module Types.MenuState where

import Delude

data MenuState
   = MainMenu
   | InGame

instance Default MenuState where def = MainMenu

