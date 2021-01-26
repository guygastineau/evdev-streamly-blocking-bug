{-# LANGUAGE LambdaCase #-}

-- | Stream integration with Streamly.
module Stream
  ( readKeyEvents
  , linesFromDevice
  ) where

import Data.Char (toUpper)

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL

import Control.Concurrent (threadDelay)

import Evdev
import Evdev.Codes

data ControlKey = Shift | Unknown
  deriving Show

data Press = Down | Up
  deriving Show

data Control = Control ControlKey Press
  deriving Show

linesFromDevice :: Device -> SerialT IO String
linesFromDevice = lines' . trClassified . readKeyEvents

readKeyEvents :: Device -> SerialT IO (Either Control Char)
readKeyEvents = S.mapMaybe ((classifyKeyEvent' =<<) . getKeyEvent) . readEvents

readEvents :: Device -> SerialT IO Event
readEvents = S.mapMaybe id . S.repeatM . nextEventMay'

nextEventMay' :: Device -> IO (Maybe Event)
nextEventMay' dev = do
  ev <- nextEventMay dev
  case ev of
    (Just _) -> return ev
    Nothing -> threadDelay 10000 >> return Nothing

-- A single type over the contents of EventData.KeyEvent
data KeyEvent' = KeyEvent' Key KeyEvent
  deriving (Show)

getKeyEvent :: Event -> Maybe KeyEvent'
getKeyEvent (Event (KeyEvent key event) _) = Just $ KeyEvent' key event
getKeyEvent _ = Nothing

lines' :: Monad m => SerialT m Char -> SerialT m String
lines' = S.splitOn (== '\n') FL.toList

classifiedReduceControlInfo
  :: Monad m => SerialT m (Either Control Char)
  -> SerialT m (Bool, Maybe Char)
classifiedReduceControlInfo = S.drop 1 . S.scanl' reduce (False, Nothing)
  where
    reduce :: (Bool, Maybe Char) -> Either Control Char -> (Bool, Maybe Char)
    reduce (mod, _) = \case
      Right c -> (mod, Just c)
      Left (Control Shift Up) -> (False, Nothing)
      Left (Control Shift Down) -> (True, Nothing)
      _ -> (mod, Nothing)

trClassified :: Monad m => SerialT m (Either Control Char) -> SerialT m Char
trClassified = S.mapMaybe tr . classifiedReduceControlInfo
  where
    tr :: (Bool, Maybe Char) -> Maybe Char
    tr (True, c) = toUpper' <$> c
    tr (False, c) = c

    toUpper' '-' = '_'
    toUpper' '=' = '+'
    toUpper' '[' = '{'
    toUpper' ']' = '}'
    toUpper' ';' = ':'
    toUpper' '\'' = '"'
    toUpper' '`' = '~'
    toUpper' ',' = '<'
    toUpper' '.' = '>'
    toUpper' '/' = '?'
    toUpper' '\\' = '|'
    toUpper'  c  = toUpper c

classifyKeyEvent' :: KeyEvent' -> Maybe (Either Control Char)
classifyKeyEvent' (KeyEvent' key event)
  | isControl key = Left . Control Shift <$> case event of
      Pressed -> Just Down
      Released -> Just Up
      Repeated -> Nothing
  | otherwise = case event of
      Pressed -> Right <$> keyToAsciiLowercase key
      _ -> Nothing
  where
    isControl :: Key -> Bool
    isControl KeyLeftshift = True
    isControl KeyRightshift = True
    isControl _ = False

keyToAsciiLowercase :: Key -> Maybe Char
keyToAsciiLowercase = \case
  Key1 -> Just '1'
  Key2 -> Just '2'
  Key3 -> Just '3'
  Key4 -> Just '4'
  Key5 -> Just '5'
  Key6 -> Just '6'
  Key7 -> Just '7'
  Key8 -> Just '8'
  Key9 -> Just '9'
  Key0 -> Just '0'
  KeyMinus -> Just '-'
  KeyEqual -> Just '='
  KeyTab -> Just '\t'
  KeyQ -> Just 'q'
  KeyW -> Just 'w'
  KeyE -> Just 'e'
  KeyR -> Just 'r'
  KeyT -> Just 't'
  KeyY -> Just 'y'
  KeyU -> Just 'u'
  KeyI -> Just 'i'
  KeyO -> Just 'o'
  KeyP -> Just 'p'
  KeyLeftbrace -> Just '['
  KeyRightbrace -> Just ']'
  KeyEnter -> Just '\n'
  KeyA -> Just 'a'
  KeyS -> Just 's'
  KeyD -> Just 'd'
  KeyF -> Just 'f'
  KeyG -> Just 'g'
  KeyH -> Just 'h'
  KeyJ -> Just 'j'
  KeyK -> Just 'k'
  KeyL -> Just 'l'
  KeySemicolon -> Just ';'
  KeyApostrophe -> Just '\''
  KeyGrave -> Just '`'
  KeyZ -> Just 'z'
  KeyX -> Just 'x'
  KeyC -> Just 'c'
  KeyV -> Just 'v'
  KeyB -> Just 'b'
  KeyN -> Just 'n'
  KeyM -> Just 'm'
  KeyComma -> Just ','
  KeyDot -> Just '.'
  KeySlash -> Just '/'
  KeySpace -> Just ' '
  KeyBackslash -> Just '\\'

  _ -> Nothing
