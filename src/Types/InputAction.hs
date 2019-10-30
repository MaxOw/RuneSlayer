{-# Language PatternSynonyms #-}
module Types.InputAction where

import Delude
import Types.Entity.Animation (AnimationKind)
import Types.EntityAction (AttackMode)
import Types.Debug (DebugFlag)
import Types.MapEditor (MapEditorAction)

data InputMode
   = NormalMode
   | InventoryMode
   | StoryDialogMode
   | RunicStatusMode
   | RunicMode
   | SpaceMode
   | StoryMode
   | MapEditorMode
   deriving (Show, Eq, Ord, Generic, Enum, Bounded)
instance Default InputMode where def = NormalMode

data MoveDirection
   = MoveUp
   | MoveDown
   | MoveLeft
   | MoveRight
   deriving (Show, Eq, Ord, Enum, Bounded, Generic)

data PanelName
   = GroundPreviewPanel
   | StatusPanel
   deriving (Eq, Ord, Show, Generic)

data TutorialAction
   = TutorialAction_SkipAll
   | TutorialAction_Restart
   deriving (Eq, Ord, Show, Generic)

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
   | SwapWeapon
   | SetAttackMode AttackMode
   | StartRunicMode
   | SelectItemToPickUp
   | SelectItemToMove
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
   deriving (Eq, Ord, Show, Generic)

--------------------------------------------------------------------------------

