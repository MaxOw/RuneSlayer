module InputKeymap where

import Delude
import Data.Char (isUpper, toLower, toUpper)

import qualified Data.Map as Map
import qualified Data.Map as PrefixMap

import Engine.Events.Types

import Types.InputAction
import Types.InputKeymap

--------------------------------------------------------------------------------

buildInputKeymap :: [InputSeq] -> KeymapDesc -> InputKeymap
buildInputKeymap cm kd = InputKeymap
    { field_keymap          = Map.fromList $ map makeInputGroup kd
    , field_keymapCommon    = buildKeymap cm
    , field_actionmap       = buildActionmap kd
    , field_actionmapCommon = acm
    }
    where
    makeInputGroup (InputGroup inputMode km) = (inputMode, buildKeymap km)
    acm = Map.fromListWith (<>) $ map (over _2 one . swap . fromInputSeq) cm

buildKeymap :: [InputSeq] -> Keymap
buildKeymap = PrefixMap.fromList . map fromInputSeq

fromInputSeq :: InputSeq -> (KeySeq, InputAction)
fromInputSeq (InputSeq iseq a) = (iseq, a)
fromInputSeq (InputStr istr a) = (parseInputStr istr, a)

buildActionmap :: [InputGroup] -> Map InputAction (Map InputMode [KeySeq])
buildActionmap
    = fmap (Map.fromListWith (<>))
    . Map.fromListWith (<>)
    . concatMap (\(InputGroup i s) -> map (swp i . fromInputSeq) s)
    where
    swp i (s, a) = (a, [(i, [s])])

-- | Lookup key sequences to press in a given InputMode for a given InputAction.
lookupInputAction :: InputAction -> InputMode -> InputKeymap -> [KeySeq]
lookupInputAction a m k
    = fromMaybe []
    $ (maybe (Map.lookup a (k^.ff#actionmapCommon)) Just)
    $ (Map.lookup m <=< Map.lookup a)
    $ view (ff#actionmap) k

showKeySeqs :: [KeySeq] -> String
showKeySeqs = \case
    []  -> "<Unbound>"
    [x] -> sks x
    xs  -> intercalate " or " $ map sks xs
    where
    sks = concatMap showKeypress

--------------------------------------------------------------------------------

showKeypress :: Keypress -> String
showKeypress (Keypress k m) = case keyToChar k of
    Just kc -> pref modk $ if mS then [toUpper kc] else [kc]
    Nothing -> pref mods $ showKey k
    where
    ModifierKeys mS mC mA mM = m
    pref p s = if null p then s else concat ["<", p, "-", s, ">"]
    mods = concatMap prettyMod [("S",mS), ("C",mC), ("A",mA), ("M",mM)]
    modk = concatMap prettyMod [          ("C",mC), ("A",mA), ("M",mM)]
    prettyMod (x,y) = if y then x else ""

parseInputStr :: [Char] -> [Keypress]
parseInputStr = mapMaybe (\c -> upShift c . KP <$> charToKey (toLower c))
-- TODO: This is too naive .. fix it

upShift :: Char -> Keypress -> Keypress
upShift c kp@(Keypress k m)
    | isUpper c = Keypress k $ m { modifierKeysShift = True }
    | otherwise = kp

--------------------------------------------------------------------------------

showKey :: Key -> String
showKey k = case keyToChar k of
    Just kc -> [kc]
    Nothing -> drop 4 $ show k

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
