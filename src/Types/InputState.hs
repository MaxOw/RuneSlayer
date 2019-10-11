{-# Language PatternSynonyms #-}
module Types.InputState (module Types.InputState) where

import Delude
import qualified Data.Set as Set
import qualified Types.Entity.Animation as Animation
import Types.Entity.Common (EntityId)
import Types.Entity.PassiveType (InteractionName)
import Types.EntityAction (AttackMode(..))
import Types.Debug (DebugFlag(..))
import Types.Equipment (EquipmentSlot)
import Engine.Events.Types
import Types.MapEditor

import Types.InputAction as Types.InputState
import Types.InputKeymap as Types.InputState
import InputKeymap

import Data.Vector (Vector)

--------------------------------------------------------------------------------

type SelectMap = PrefixMap Char Int

data SelectValues a = SelectValues
   { field_values  :: Vector a
   , field_hintMap :: Map a [Char]
   } deriving (Show, Generic)
instance Default (SelectValues a)

data ItemMoveTarget
   = ItemMoveTarget_EquipmentSlot EquipmentSlot
   | ItemMoveTarget_Backpack
   | ItemMoveTarget_Container
   | ItemMoveTarget_Ground
   deriving (Eq, Ord, Show, Generic)

data SelectKind
   = SelectKind_Pickup   (SelectValues EntityId)
   | SelectKind_Drop     (SelectValues EntityId)
   | SelectKind_Focus    (SelectValues EntityId)
   | SelectKind_MoveTo   (SelectValues ItemMoveTarget)
   | SelectKind_Action   (SelectValues (EntityId, InteractionName))
   deriving (Show, Generic)

--------------------------------------------------------------------------------

data ActiveAction
   = ActiveMove MoveDirection
   deriving (Eq, Ord)

data SelectState = SelectState
   { field_selectKind    :: SelectKind
   , field_selectMap     :: SelectMap
   , field_currentPrefix :: Seq Char
   } deriving (Generic)

data InventoryState = InventoryState
   { field_focusedItem :: Maybe EntityId
   , field_container   :: Maybe EntityId
   , field_moveStart   :: Maybe EntityId
   } deriving (Generic)

instance Default InventoryState

data InputState = InputState
   { field_mode           :: InputMode
   , field_hist           :: Seq Keypress
   , field_active         :: Map ActiveAction Int
   , field_inputString    :: Seq Char
   , field_inputKeymap    :: InputKeymap
   , field_deactivators   :: Map Key [InputAction]
   , field_selectState    :: Maybe SelectState
   , field_visiblePanels  :: Set PanelName
   , field_inventoryState :: InventoryState
   } deriving (Generic)

type InputStateM = StateT InputState IO

--------------------------------------------------------------------------------

defaultInputState :: InputState
defaultInputState = InputState
    { field_mode           = StoryMode
    , field_hist           = def
    , field_active         = def
    , field_inputString    = def
    , field_inputKeymap    = defaultInputKeymap
    , field_deactivators   = def
    , field_selectState    = def
    , field_visiblePanels  = defaultVisiblePanels
    , field_inventoryState = def
    }

defaultVisiblePanels :: Set PanelName
defaultVisiblePanels = Set.fromList
    [ StatusPanel
    ]

defaultSelectors :: [Char]
defaultSelectors = "jfkdlsahgurieowpq"
-- defaultSelectors = "jf"

defaultCommonInputSeqs :: [InputSeq]
defaultCommonInputSeqs =
    [ InputKey Key'Escape InputAction_Escape
    ]

defaultInputKeymap :: InputKeymap
defaultInputKeymap = buildInputKeymap defaultCommonInputSeqs
    [ InputGroup NormalMode
        -- [ InputKey Key'Space  (SetMode SpaceMode)

        [ InputStr "i" (SetMode InventoryMode)

        , InputStr "j" (SimpleMove MoveDown)
        , InputStr "k" (SimpleMove MoveUp)
        , InputStr "h" (SimpleMove MoveLeft)
        , InputStr "l" (SimpleMove MoveRight)

        , InputStr "tr" (ToggleDebug DebugFlag_DrawPickupRange)
        , InputStr "tg" (ToggleViewPanel GroundPreviewPanel)
        , InputStr "ts" (ToggleDebug DebugFlag_ZoomOutScroller)
        , InputStr "tb" (ToggleDebug DebugFlag_HideScroller)
        , InputStr "td" (ToggleDebug DebugFlag_ShowDynamicBoundingBoxes)
        , InputStr "tc" (ToggleDebug DebugFlag_ShowCollisionShapes)

        , InputStr "Dac" (DebugRunAnimation Animation.Cast)
        , InputStr "Dat" (DebugRunAnimation Animation.Thrust)
        , InputStr "Daw" (DebugRunAnimation Animation.Walk)
        , InputStr "Das" (DebugRunAnimation Animation.Slash)
        , InputStr "Daf" (DebugRunAnimation Animation.Fire)
        , InputStr "Dad" (DebugRunAnimation Animation.Die)

        , InputStr "Q" FastQuit

        , InputStr "yy" PickupAllItems
        -- , InputStr "yi" SelectItemToPickUp
        , InputStr "pp" DropAllItems

        , InputStr "g" ExecuteAttack

        , InputStr "ss" SwapWeapon

        , InputStr "mm" (SetAttackMode AttackMode_Manual)
        , InputStr "ma" (SetAttackMode AttackMode_Auto)

        , InputStr "M" (SetMode MapEditorMode)

        , InputStr "r" StartRunicMode

        , InputStr "f" Interact
        , InputStr "F" SelectInteraction

        , InputStr "Ts" (TutorialAction TutorialAction_SkipAll)
        , InputStr "Tr" (TutorialAction TutorialAction_Restart)
        ]

    , InputGroup InventoryMode
        [ InputStr "yy" PickupAllItems
        , InputStr "yi" SelectItemToPickUp

        , InputStr "pp" DropAllItems
        , InputStr "pi" SelectItemToDrop

        , InputStr "m" SelectItemMoveTarget

        , InputStr "f" SelectItemToFocus
        , InputStr "u" UseFocusedItem

        , InputStr "ss" SwapWeapon
        ]

    , InputGroup StoryDialogMode
        [ InputKey Key'Escape InputAction_Nothing
        , InputKey Key'Space  InputAction_NextPage
        , InputStr "Q" FastQuit
        ]

    , InputGroup StoryMode
        [ InputKey Key'Escape InputAction_Nothing
        , InputStr "Q" FastQuit
        ]

    , InputGroup MapEditorMode
        [ InputStr "j" (SimpleMove MoveDown)
        , InputStr "k" (SimpleMove MoveUp)
        , InputStr "h" (SimpleMove MoveLeft)
        , InputStr "l" (SimpleMove MoveRight)

        , InputStr "n" (MapEditorAction MapEditorAction_SelectNext)
        , InputStr "p" (MapEditorAction MapEditorAction_SelectPrev)
        , InputStr "m" (MapEditorAction MapEditorAction_NextCategory)
        , InputKey Key'Space (MapEditorAction MapEditorAction_PlaceEntity)

        , InputStr "Q" FastQuit
        ]
    ]


