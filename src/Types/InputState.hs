{-# Language TemplateHaskell #-}
{-# Language PatternSynonyms #-}
module Types.InputState where

import Delude
import qualified Prelude (Show(..))
import Data.Char (isUpper, toLower)
import qualified Data.Map as Map
import qualified Data.Map as PrefixMap
import Types.Entity.Common (EntityId)
import Types.Debug (DebugFlag(..))
import Engine.Events.Types
import qualified Control.Monad.Trans.State.Lazy as Lazy
-- import Engine (defaultModifierKeys)

import Data.Vector (Vector)

data StatusMenu
   = StatusMenu_Inventory
   -- | StatusMenu_Stats
   deriving (Show, Eq, Ord, Generic)

pattern Inventory = StatusMenu_Inventory

type SelectMap = PrefixMap Char Int

data SelectValues a = SelectValues
   { selectValues_values  :: Vector a
   , selectValues_hintMap :: Map a [Char]
   } deriving (Show, Generic)
instance Default (SelectValues a)

data SelectKind
   = SelectPickup (SelectValues EntityId)
   | SelectDrop   (SelectValues EntityId)
   | SelectFocus  (SelectValues EntityId)
   deriving (Show, Generic)

data InputMode
   = NormalMode
   | FightMode
   | DefendMode
   | StatusMode StatusMenu
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
   deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

data InputAction
   = SetMode InputMode
   | SimpleMove MoveDirection
   | ToggleDebug DebugFlag
   | ToggleViewPanel PanelName
   | PickupAllItems
   | DropAllItems
   | SelectItemToPickUp
   | SelectItemToDrop
   | SelectItemToFocus
   | InputAction_Escape
   | FastQuit
   deriving (Show)

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
   { selectState_selectKind    :: SelectKind
   , selectState_selectMap     :: SelectMap
   , selectState_currentPrefix :: Seq Char
   } deriving (Generic)
makeFieldsCustom ''SelectState

data InventoryState = InventoryState
   { inventoryState_focusedItem :: Maybe EntityId
   } deriving (Generic)
makeFieldsCustom ''InventoryState
instance Default InventoryState

data InputState = InputState
   { inputState_mode           :: InputMode
   , inputState_hist           :: Seq Keypress
   , inputState_active         :: Map ActiveAction Int
   , inputState_commonKeymap   :: Keymap
   , inputState_inputKeymap    :: InputKeymap
   , inputState_deactivators   :: Map Keypress [InputAction]
   , inputState_selectState    :: Maybe SelectState
   , inputState_visiblePanels  :: Set PanelName
   , inputState_inventoryState :: InventoryState
   } deriving (Generic)
makeFieldsCustom ''InputState
instance Default InputState where
    def = InputState
        { inputState_mode           = def
     -- { inputState_mode           = StatusMode StatusMenu_Inventory
        , inputState_hist           = def
        , inputState_active         = def
        , inputState_commonKeymap   = defaultCommonKeymap
        , inputState_inputKeymap    = defaultInputKeymap
        , inputState_deactivators   = def
        , inputState_selectState    = Nothing
        , inputState_visiblePanels  = def
        , inputState_inventoryState = def
        }

type InputStateM = Lazy.StateT InputState IO

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
-- defaultSelectors = "jfkdlsahgurieowpq"
defaultSelectors = "jf"

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
        [ InputKey Key'F      (SetMode FightMode)
        , InputKey Key'Space  (SetMode SpaceMode)

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

        , InputStr "q" FastQuit

        , InputStr "yy" PickupAllItems
        -- , InputStr "yi" SelectItemToPickUp

        , InputStr "pp" DropAllItems
        ]

    , InputGroup FightMode
        [ -- InputKey Key'Escape (SetMode NormalMode)
        ]

    , InputGroup SpaceMode
        [ -- InputKey Key'Escape (SetMode NormalMode)
        ]

    , InputGroup (StatusMode Inventory)
        [ InputStr "yy" PickupAllItems
        , InputStr "yi" SelectItemToPickUp

        , InputStr "pp" DropAllItems
        , InputStr "pi" SelectItemToDrop

        , InputStr "f" SelectItemToFocus
        ]
    ]

makeFieldsCustom ''SelectValues
