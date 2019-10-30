{-# OPTIONS_GHC -fno-warn-orphans #-}
module InputKeymap
    ( buildInputKeymap
    , lookupInputAction
    , showKeySeqs
    , keyToChar
    , defaultBindings
    ) where

import Delude hiding (some, many)
import Data.Char (isUpper, toLower, toUpper)

import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict as PrefixMap

import Engine.Events.Types

import Types.InputAction
import Types.InputKeymap
import InputKeymap.Default

import Text.Megaparsec
import Text.Megaparsec.Char

--------------------------------------------------------------------------------

buildInputKeymap :: [KeymapEntry Text] -> Either [Text] InputKeymap
buildInputKeymap ks = case es of
    [] -> Right $ InputKeymap
        { field_keymap    = km
        , field_actionmap = foldActionmap km
        }
    _ -> Left es
    where
    km = foldKeymap ps
    (es, ps) = partitionEithers $ map parseEntry ks

foldKeymap :: [KeymapEntry KeySeq] -> Map InputMode Keymap
foldKeymap = foldl' applyEntry mempty
    where
    applyEntry m e = foldl' alterMode m modes
        where
        modes = fromMaybe boundedRange $ fmap one $ e^.mode
        alterMode = flip $ Map.alter (Just . alterKeymap . fromMaybe mempty)
        alterKeymap = PrefixMap.alter (const $ e^.ff#action) (e^.ff#keyseq)

foldActionmap :: Map InputMode Keymap -> (Map InputAction (Map InputMode [KeySeq]))
foldActionmap mm = foldl' applyEntry mempty im
    where
    im :: [(InputMode, KeySeq, InputAction)]
    im = concatMap (f . over _2 Map.toList) $ Map.toList mm
    f (m, ks) = map (uncurry (m,,)) ks
    applyEntry m (md, ks, ia) = Map.alter alterAction ia m
        where
        alterAction = Just . alterKeymap . fromMaybe mempty
        alterKeymap = Map.insertWith (<>) md [ks]

parseEntry :: KeymapEntry Text -> Either Text (KeymapEntry KeySeq)
parseEntry k = let kt = k^.ff#keyseq in case parseKeyseq kt of
    Nothing -> Left kt
    Just ks -> Right $ k { field_keyseq = ks }

--------------------------------------------------------------------------------

instance Semigroup ModifierKeys where
    ModifierKeys aS aC aA aM <> ModifierKeys bS bC bA bM
        = ModifierKeys (aS || bS) (aC || bC) (aA || bA) (aM || bM)
instance Monoid ModifierKeys where
    mempty = ModifierKeys False False False False

modShift :: ModifierKeys
modShift = ModifierKeys True False False False

modCtrl :: ModifierKeys
modCtrl = ModifierKeys False True False False

modAlt :: ModifierKeys
modAlt = ModifierKeys False False True False

modMeta :: ModifierKeys
modMeta = ModifierKeys False False False True

--------------------------------------------------------------------------------

parseKeyseq :: Text -> Maybe [Keypress]
parseKeyseq = parseMaybe parser

type Parser = Parsec Void Text

parser :: Parser [Keypress]
parser = some keyp

keyp :: Parser Keypress
keyp = specialKey <|> charKey

specialKey :: Parser Keypress
specialKey = between (char '<') (char '>') specialKeyInside

specialKeyInside :: Parser Keypress
specialKeyInside = do
    pp <- mconcat <$> many (try modPrefix)
    Keypress kp km <- choice
        [ spec "bs"        Key'Backspace
        , spec "backspace" Key'Backspace
        , spec "tab"       Key'Tab
        , spec "cr"        Key'Enter
        , spec "enter"     Key'Enter
        , spec "return"    Key'Enter
        , spec "escape"    Key'Escape
        , spec "esc"       Key'Escape
        , spec "space"     Key'Space
        , spec "delete"    Key'Delete
        , spec "del"       Key'Delete
        , spec "pause"     Key'Pause
        , spec "up"        Key'Up
        , spec "down"      Key'Down
        , spec "left"      Key'Left
        , spec "right"     Key'Right

        , spec "f2"        Key'F2
        , spec "f3"        Key'F3
        , spec "f4"        Key'F4
        , spec "f5"        Key'F5
        , spec "f6"        Key'F6
        , spec "f7"        Key'F7
        , spec "f8"        Key'F8
        , spec "f9"        Key'F9
        , spec "f10"       Key'F10
        , spec "f11"       Key'F11
        , spec "f12"       Key'F12
        , spec "f1"        Key'F1

        , charKey
        ]

    return $ Keypress kp (km <> pp)

spec :: Text -> Key -> Parser Keypress
spec s k = string' s >> pure (Keypress k mempty)

modPrefix :: Parser ModifierKeys
modPrefix = choice
    [ pref 's' modShift
    , pref 'c' modCtrl
    , pref 'a' modAlt
    , pref 'm' modMeta
    ]
    where
    pref c v = char' c >> char '-' >> pure v

charKey :: Parser Keypress
charKey = do
    c <- asciiChar
    let is = bool mempty modShift $ isUpper c
    case charToKey $ toLower c of
        Nothing -> fail $ "Unable to parse char: " <> show c
        Just ck -> return $ Keypress ck is

charToKey :: Char -> Maybe Key
charToKey = \case
  '\'' -> Just Key'Apostrophe
  ','  -> Just Key'Comma
  '-'  -> Just Key'Minus
  '.'  -> Just Key'Period
  '/'  -> Just Key'Slash
  '0'  -> Just Key'0
  '1'  -> Just Key'1
  '2'  -> Just Key'2
  '3'  -> Just Key'3
  '4'  -> Just Key'4
  '5'  -> Just Key'5
  '6'  -> Just Key'6
  '7'  -> Just Key'7
  '8'  -> Just Key'8
  '9'  -> Just Key'9
  ';'  -> Just Key'Semicolon
  '='  -> Just Key'Equal
  'a'  -> Just Key'A
  'b'  -> Just Key'B
  'c'  -> Just Key'C
  'd'  -> Just Key'D
  'e'  -> Just Key'E
  'f'  -> Just Key'F
  'g'  -> Just Key'G
  'h'  -> Just Key'H
  'i'  -> Just Key'I
  'j'  -> Just Key'J
  'k'  -> Just Key'K
  'l'  -> Just Key'L
  'm'  -> Just Key'M
  'n'  -> Just Key'N
  'o'  -> Just Key'O
  'p'  -> Just Key'P
  'q'  -> Just Key'Q
  'r'  -> Just Key'R
  's'  -> Just Key'S
  't'  -> Just Key'T
  'u'  -> Just Key'U
  'v'  -> Just Key'V
  'w'  -> Just Key'W
  'x'  -> Just Key'X
  'y'  -> Just Key'Y
  'z'  -> Just Key'Z
  '('  -> Just Key'LeftBracket
  '\\' -> Just Key'Backslash
  ')'  -> Just Key'RightBracket
  '`'  -> Just Key'GraveAccent
  _    -> Nothing

--------------------------------------------------------------------------------

-- | Lookup key sequences to press in a given InputMode for a given InputAction.
lookupInputAction :: InputAction -> InputMode -> InputKeymap -> [KeySeq]
lookupInputAction a m k
    = fromMaybe []
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

--------------------------------------------------------------------------------

showKey :: Key -> String
showKey k = case keyToChar k of
    Just kc -> [kc]
    Nothing -> drop 4 $ show k

keyToChar :: Key -> Maybe Char
keyToChar = flip Map.lookup keyCharMap

--------------------------------------------------------------------------------

keyCharMap :: Map Key Char
keyCharMap = Map.fromList charKeyMapping

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
