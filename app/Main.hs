{-# LANGUAGE OverloadedLabels , OverloadedLists, OverloadedStrings #-}

module Main where

import Data.Text     (Text)
import Data.Function ((&))

import Pipes
import qualified Pipes.Extras as Pipes

import Control.Concurrent.Async (async)
import Control.Monad (void)
import Data.ByteString (ByteString)

import Control.Arrow hiding (app)

import GI.Gtk (Label(..), Window(..))
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk

data State = Initial | Greeting Text Text

data Event = Greet Text | Color Text | Closed

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
                   Greeting who color -> widget Label [ classes ["big-text", color]
                                                , #label := who
                                                ]

update' :: State -> Event -> Transition State Event
update' Initial            (Greet who)   = Transition (Greeting who "blue") (return Nothing)
update' Initial            (Color color) = Transition (Greeting "no one" color) (return Nothing)
update' (Greeting _ color) (Greet who)   = Transition (Greeting who color) (return Nothing)
update' (Greeting who _)   (Color color) = Transition (Greeting who color) (return Nothing)
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
    greetings
      = cycle ["Joe", "Mike"]
      & map (Greet . ("Hello, " <>))
      & each
      & (>-> Pipes.delay 2.0)

    colors
      = cycle ["purple", "green", "blue"]
      & map Color
      & each
      & (>-> Pipes.delay 1.0)

    app = App { view = view'
              , update = update'
              , inputs = [greetings, colors]
              , initialState = Initial
              }
