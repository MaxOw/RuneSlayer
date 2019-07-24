{-# Language PatternSynonyms #-}
module Types.InputState where

import Delude
import qualified Prelude (Show(..))
import Data.Char (isUpper, toLower)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Map as PrefixMap
import qualified Types.Entity.Animation as Animation
import Types.Entity.Animation (AnimationKind)
import Types.Entity.Common (EntityId)
import Types.Entity.PassiveType (UseActionName)
import Types.EntityAction (AttackMode(..))
import Types.Debug (DebugFlag(..))
import Types.Equipment (EquipmentSlot)
import Engine.Events.Types
-- import Engine (defaultModifierKeys)

import Data.Vector (Vector)

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
   | InputAction_Escape
   | FastQuit
   deriving (Show)

--------------------------------------------------------------------------------

data StatusMenu
   = StatusMenu_Inventory
   -- | StatusMenu_Stats
   deriving (Show, Eq, Ord, Generic)

pattern Inventory = StatusMenu_Inventory

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

data InputMode
   = NormalMode
   | StatusMode StatusMenu
   | OffensiveMode
   | DefensiveMode
   | SpaceMode
   deriving (Show, Eq, Ord, Generic)
instance Default InputMode where def = NormalMode

data Keypress = Keypress
   { keypressKey :: Key
   , keypressMod :: ModifierKeys
   } deriving (Generic, Eq, Ord)
instance Show Keypress where
    show (Keypress k m) = pref <> show k
        where
        ModifierKeys mS mC mA mM = m
        pref = if not $ null modp then modp <> "+" else ""
        modp = concatMap prettyMod [("S",mS), ("C",mC), ("A",mA), ("M",mM)]
        prettyMod (x,y) = if y then x else ""

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

data ActiveAction
   = ActiveMove MoveDirection
   deriving (Eq, Ord)

data InputGroup = InputGroup InputMode [InputSeq]
data InputSeq
   = InputSeq [Keypress] InputAction
   | InputStr [Char]     InputAction
   -- | InputPrefix [Keypress] (Maybe InputAction) [InputSeq]

pattern KP :: Key -> Keypress
pattern KP k = Keypress k (ModifierKeys False False False False)

pattern InputKey :: Key -> InputAction -> InputSeq
pattern InputKey k a = InputSeq [KP k] a

type PrefixMap k v = Map [k] v
type KeymapDesc = [InputGroup]
type Keymap = PrefixMap Keypress InputAction
type InputKeymap = Map InputMode Keymap

--------------------------------------------------------------------------------

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
        }

defaultVisiblePanels :: Set PanelName
defaultVisiblePanels = Set.fromList
    [ StatusPanel
    , OffensiveSlotsPanel
    , DefensiveSlotsPanel
    ]

type InputStateM = StateT InputState IO

--------------------------------------------------------------------------------

buildInputKeymap :: KeymapDesc -> InputKeymap
buildInputKeymap = Map.fromList . map makeInputGroup
    where
    makeInputGroup (InputGroup inputMode km) = (inputMode, buildKeymap km)


buildKeymap :: [InputSeq] -> Keymap
buildKeymap = PrefixMap.fromList . map toInputSeq
    where
    toInputSeq (InputSeq iseq a) = (iseq, a)
    toInputSeq (InputStr istr a) = (parseInputStr istr, a)

parseInputStr :: [Char] -> [Keypress]
parseInputStr = mapMaybe (\c -> upShift c . KP <$> charToKey (toLower c))
-- TODO: This is too naive .. fix it

upShift :: Char -> Keypress -> Keypress
upShift c kp@(Keypress k m)
    | isUpper c = Keypress k $ m { modifierKeysShift = True }
    | otherwise = kp

defaultSelectors :: [Char]
defaultSelectors = "jfkdlsahgurieowpq"
-- defaultSelectors = "jf"

--------------------------------------------------------------------------------

charToKey :: Char -> Maybe Key
charToKey = flip Map.lookup charKeyMap

keyToChar :: Key -> Maybe Char
keyToChar = flip Map.lookup keyCharMap

--------------------------------------------------------------------------------

keyCharMap :: Map Key Char
keyCharMap = Map.fromList charKeyMapping

charKeyMap :: Map Char Key
charKeyMap = Map.fromList . map swap $ charKeyMapping

charKeyMapping :: [(Key, Char)]
charKeyMapping =
    [ (Key'A, 'a')
    , (Key'B, 'b')
    , (Key'C, 'c')
    , (Key'D, 'd')
    , (Key'E, 'e')
    , (Key'F, 'f')
    , (Key'G, 'g')
    , (Key'H, 'h')
    , (Key'I, 'i')
    , (Key'J, 'j')
    , (Key'K, 'k')
    , (Key'L, 'l')
    , (Key'M, 'm')
    , (Key'N, 'n')
    , (Key'O, 'o')
    , (Key'P, 'p')
    , (Key'Q, 'q')
    , (Key'R, 'r')
    , (Key'S, 's')
    , (Key'T, 't')
    , (Key'U, 'u')
    , (Key'V, 'v')
    , (Key'W, 'w')
    , (Key'X, 'x')
    , (Key'Y, 'y')
    , (Key'Z, 'z')
    ]

--------------------------------------------------------------------------------

defaultCommonKeymap :: Keymap
defaultCommonKeymap = buildKeymap
    [ InputKey Key'Escape InputAction_Escape
    ]

defaultInputKeymap :: InputKeymap
defaultInputKeymap = buildInputKeymap
    [ InputGroup NormalMode
        [ InputKey Key'Space  (SetMode SpaceMode)

        , InputStr "i" (SetMode $ StatusMode Inventory)

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

        , InputStr "gf" ExecuteAttack

        , InputStr "mm" (SetAttackMode AttackMode_Manual)
        , InputStr "ma" (SetAttackMode AttackMode_Auto)

        , InputStr "f" StartOffensiveMode
        , InputStr "d" StartDefensiveMode

        , InputStr "a" SelectAction
        ]

    , InputGroup (StatusMode Inventory)
        [ InputStr "yy" PickupAllItems
        , InputStr "yi" SelectItemToPickUp

        , InputStr "pp" DropAllItems
        , InputStr "pi" SelectItemToDrop

        , InputStr "m" SelectItemMoveTarget

        , InputStr "f" SelectItemToFocus
        , InputStr "u" UseFocusedItem
        ]
    ]


