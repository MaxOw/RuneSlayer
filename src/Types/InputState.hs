{-# Language PatternSynonyms #-}
module Types.InputState (module Types.InputState) where

import Delude
import qualified Data.Set as Set
import Types.Entity.Common (EntityId)
import Types.Entity.PassiveType (InteractionName)
import Types.Equipment (EquipmentSlot)
import Engine.Events.Types

import Types.InputAction as Types.InputState
import Types.InputKeymap as Types.InputState

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
   | SelectKind_Move     (SelectValues EntityId)
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
    , field_inputKeymap    = def
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

