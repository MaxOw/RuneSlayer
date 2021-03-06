{-# Language PatternSynonyms #-}
module Types.InputKeymap where

import Delude
import qualified Prelude (Show(..))
import Engine.Events.Types
import Types.InputAction

--------------------------------------------------------------------------------

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
type KeySeq = [Keypress]

--------------------------------------------------------------------------------

data InputKeymap = InputKeymap
   { field_keymap    :: Map InputMode Keymap
   , field_actionmap :: Map InputAction (Map InputMode [KeySeq])
   } deriving (Generic)
instance Default InputKeymap

data KeymapEntry a = KeymapEntry
   { field_mode   :: Maybe InputMode
   , field_keyseq :: a
   , field_action :: Maybe InputAction
   } deriving (Generic)
