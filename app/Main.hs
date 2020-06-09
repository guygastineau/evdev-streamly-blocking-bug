{-# LANGUAGE
    OverloadedLabels
  , OverloadedLists
  , OverloadedStrings
  #-}

module Main where

--import Lib

import GI.Gtk (Label(..), Window(..))
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

main :: IO ()
main = putStrLn "Hello"
