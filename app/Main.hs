{-# LANGUAGE
    OverloadedLabels
  , OverloadedLists
  , OverloadedStrings
  #-}

module Main where

import Data.Text     (Text)
import Data.Function ((&))
import Control.Monad (void)

import Pipes
import qualified Pipes.Extras as Pipes

import GI.Gtk (Label(..), Window(..))
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

data State = Initial | Greeting Text

data Event = Greet Text | Closed

view' :: State -> AppView Window Event
view' s =
  bin Window [ #title := "Hello"
             , on #deleteEvent (const (True, Closed))
             , #widthRequest := 400
             , #heightRequest := 300
             ] $ case s of
                   Initial      -> widget Label [#label := "Nothing here yet."]
                   Greeting who -> widget Label [#label := who]

update' :: State -> Event -> Transition State Event
update' _ (Greet who) = Transition (Greeting who) (return Nothing)
update' _ Closed      = Exit

main :: IO ()
main = void $ run App { view = view'
                      , update = update'
                      , inputs = [greetings]
                      , initialState = Initial
                      }
  where
    greetings
      = cycle ["Joe", "Mike"]
      & map (\n -> Greet ("Hello, " <> n))
      & each
      & (>-> Pipes.delay 1.0)
