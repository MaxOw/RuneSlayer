{-# Language PatternSynonyms #-}
module Types.InputAction where

import Delude
import Types.Entity.Animation (AnimationKind)
import Types.EntityAction (AttackMode)
import Types.Debug (DebugFlag)
import Types.MapEditor (MapEditorAction)

data StatusMenu
   = StatusMenu_Inventory
   | StatusMenu_StoryDialog
   -- | StatusMenu_Stats
   deriving (Show, Eq, Ord, Generic)

data InputMode
   = NormalMode
   | StatusMode StatusMenu
   | RunicMode
   | SpaceMode
   | MapEditorMode
   deriving (Show, Eq, Ord, Generic)
instance Default InputMode where def = NormalMode

pattern InventoryMode   = StatusMode StatusMenu_Inventory
pattern StoryDialogMode = StatusMode StatusMenu_StoryDialog

data MoveDirection
   = MoveUp
   | MoveDown
   | MoveLeft
   | MoveRight
   deriving (Show, Eq, Ord, Enum, Bounded)

data PanelName
   = GroundPreviewPanel
   | StatusPanel
   deriving (Eq, Ord, Show)

data TutorialAction
   = TutorialAction_SkipAll
   | TutorialAction_Restart
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
   | StartRunicMode
   | SelectItemToPickUp
   | SelectItemMoveTarget
   | SelectItemToDrop
   | SelectItemToFocus
   | UseFocusedItem
   | SelectInteraction
   | Interact
   | FastQuit
   | MapEditorAction MapEditorAction
   | TutorialAction TutorialAction
   | InputAction_NextPage
   | InputAction_Escape
   | InputAction_Nothing
   deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

