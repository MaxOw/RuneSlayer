{-# Language PatternSynonyms #-}
module Types.InputAction where

import Delude
import Types.Entity.Animation (AnimationKind)
import Types.EntityAction (AttackMode(..))
import Types.Debug (DebugFlag(..))

data StatusMenu
   = StatusMenu_Inventory
   | StatusMenu_StoryDialog
   -- | StatusMenu_Stats
   deriving (Show, Eq, Ord, Generic)

data InputMode
   = NormalMode
   | StatusMode StatusMenu
   | OffensiveMode
   | DefensiveMode
   | SpaceMode
   deriving (Show, Eq, Ord, Generic)
instance Default InputMode where def = NormalMode

pattern InventoryMode   = StatusMode StatusMenu_Inventory
pattern StoryDialogMode = StatusMode StatusMenu_StoryDialog

data MoveDirection
   = MoveUp
   | MoveDown
   | MoveLeft
   | MoveRight
   deriving (Show, Eq, Ord)

data PanelName
   = GroundPreviewPanel
   | StatusPanel
   | OffensiveSlotsPanel
   | DefensiveSlotsPanel
   deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

data InputAction
   = SetMode InputMode
   | SimpleMove MoveDirection
   | ToggleDebug DebugFlag
   | DebugRunAnimation AnimationKind
   | ToggleViewPanel PanelName
   | PickupAllItems
   | DropAllItems
   | ExecuteAttack
   | SetAttackMode AttackMode
   | StartOffensiveMode
   | StartDefensiveMode
   | SelectItemToPickUp
   | SelectItemMoveTarget
   | SelectItemToDrop
   | SelectItemToFocus
   | UseFocusedItem
   | SelectAction
   | FastQuit
   | InputAction_NextPage
   | InputAction_Escape
   | InputAction_Nothing
   deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

