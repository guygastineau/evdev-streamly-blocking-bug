{-# LANGUAGE OverloadedLabels , OverloadedLists, OverloadedStrings #-}

module Main where

import Evdev (Device)
import Producers
import System.Environment

import Data.Text     (Text, pack)
import Data.Function ((&))

import Pipes
import qualified Pipes.Extras as Pipes

import Control.Concurrent.Async (async)
import Control.Monad (fail, void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)

import GI.Gtk (Label(..), Window(..))
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk

data State = Initial | Display Text Text

data Event = Swipe Text | Color Text | Closed

view' :: State -> AppView Window Event
view' s =
  bin Window [ #title := "Hello"
             , on #deleteEvent (const (True, Closed))
             , #widthRequest := 400
             , #heightRequest := 300
             ] $ case s of
                   Initial            -> widget Label [ classes ["big-text"]
                                                , #label := "Nothing here yet."
                                                ]
                   Display code color -> widget Label [ classes ["big-text", color]
                                                , #label := code
                                                ]

update' :: State -> Event -> Transition State Event
update' Initial           (Swipe code)  = Transition (Display code "blue") (return Nothing)
update' Initial           (Color color) = Transition (Display "no one" color) (return Nothing)
update' (Display _ color) (Swipe code)  = Transition (Display code color) (return Nothing)
update' (Display code _)  (Color color) = Transition (Display code color) (return Nothing)
update' _  Closed = Exit

styles :: ByteString
styles = mconcat
  [ ".big-text { font-size: 24px; }"
  , ".red { color: red; }"
  , ".purple { color: purple; }"
  , ".blue { color: blue; }"
  , ".green { color: green; }"
  , "window { background-color: #333; }"
  ]

main :: IO ()
main = do
  args <- getArgs
  dev <- if length args == 1 then getDevice $ head args
         else fail "One and only one argument is required!"

  let app = App { view = view'
                , update = update'
                , inputs = [swipes dev, colors]
                , initialState = Initial
                }

  void $ Gtk.init Nothing

  -- Set up screen and and CSS provider
  screen <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault
  p      <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData p styles
  Gtk.styleContextAddProviderForScreen
    screen
    p
    (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

  -- Start main loop
  void . async $ do
    void $ runLoop app
    Gtk.mainQuit
  Gtk.main
  where
    swipes :: Device -> Producer Event IO ()
    swipes = toPipes . streamDeviceWith (Swipe . pack)

    colors
      = cycle ["purple", "green", "blue"]
      & map Color
      & each
      & (>-> Pipes.delay 3.0)
