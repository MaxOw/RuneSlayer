{-# Language PatternSynonyms #-}
module Types.InputState (module Types.InputState) where

import Delude
import qualified Data.Set as Set
import qualified Types.Entity.Animation as Animation
import Types.Entity.Common (EntityId)
import Types.Entity.PassiveType (UseActionName)
import Types.EntityAction (AttackMode(..))
import Types.Debug (DebugFlag(..))
import Types.Equipment (EquipmentSlot)
import Engine.Events.Types
-- import Engine (defaultModifierKeys)

import Types.InputAction as Types.InputState
import Types.InputKeymap as Types.InputState
import InputKeymap

import Data.Zipper (Zipper)
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
   | SelectKind_Action   (SelectValues (EntityId, UseActionName))
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

-- data RunicState = RunicState

instance Default InventoryState

data StoryDialogState = StoryDialogState
    { field_title       :: Text
    , field_entityId    :: EntityId
    , field_dialogPages :: Zipper Text
    } deriving (Generic)
instance HasEntityId StoryDialogState EntityId

data InputState = InputState
   { field_mode           :: InputMode
   , field_hist           :: Seq Keypress
   , field_active         :: Map ActiveAction Int
   , field_inputString    :: Seq Char
   , field_commonKeymap   :: Keymap
   , field_inputKeymap    :: InputKeymap
   , field_deactivators   :: Map Keypress [InputAction]
   , field_selectState    :: Maybe SelectState
   -- , field_offensiveState :: Maybe RunicState
   , field_visiblePanels  :: Set PanelName
   , field_inventoryState :: InventoryState
   , field_storyDialog    :: Maybe StoryDialogState
   } deriving (Generic)

instance Default InputState where
    def = InputState
        { field_mode           = def
        , field_hist           = def
        , field_active         = def
        , field_inputString    = def
        , field_commonKeymap   = defaultCommonKeymap
        , field_inputKeymap    = defaultInputKeymap
        , field_deactivators   = def
        , field_selectState    = def
        -- , field_offensiveState = def
        , field_visiblePanels  = defaultVisiblePanels
        , field_inventoryState = def
        , field_storyDialog    = def
        }

defaultVisiblePanels :: Set PanelName
defaultVisiblePanels = Set.fromList
    [ StatusPanel
    , OffensiveSlotsPanel
    , DefensiveSlotsPanel
    ]

type InputStateM = StateT InputState IO

--------------------------------------------------------------------------------

defaultSelectors :: [Char]
defaultSelectors = "jfkdlsahgurieowpq"
-- defaultSelectors = "jf"

defaultCommonKeymap :: Keymap
defaultCommonKeymap = buildKeymap
    [ InputKey Key'Escape InputAction_Escape
    ]

defaultInputKeymap :: InputKeymap
defaultInputKeymap = buildInputKeymap
    [ InputGroup NormalMode
        [ InputKey Key'Space  (SetMode SpaceMode)

        , InputStr "i" (SetMode InventoryMode)

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

        , InputStr "q" FastQuit

        , InputStr "yy" PickupAllItems
        -- , InputStr "yi" SelectItemToPickUp
        , InputStr "pp" DropAllItems

        , InputStr "g" ExecuteAttack

        , InputStr "mm" (SetAttackMode AttackMode_Manual)
        , InputStr "ma" (SetAttackMode AttackMode_Auto)

        , InputStr "f" StartOffensiveMode
        , InputStr "d" StartDefensiveMode

        , InputStr "a" SelectAction
        ]

    , InputGroup InventoryMode
        [ InputStr "yy" PickupAllItems
        , InputStr "yi" SelectItemToPickUp

        , InputStr "pp" DropAllItems
        , InputStr "pi" SelectItemToDrop

        , InputStr "m" SelectItemMoveTarget

        , InputStr "f" SelectItemToFocus
        , InputStr "u" UseFocusedItem
        ]

    , InputGroup StoryDialogMode
        [ InputKey Key'Escape InputAction_Nothing
        , InputKey Key'Space  InputAction_NextPage
        ]
    ]


