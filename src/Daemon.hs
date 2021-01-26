module Daemon
    ( runDaemon
    ) where

import Evdev
import Stream

import Control.Monad.IO.Class (liftIO)

import qualified Streamly.Prelude as S

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

runDaemon :: String -> IO ()
runDaemon name = do
  dev <- newDevice . encodeUtf8 . T.pack $ name
  grabDevice dev
  liftIO . S.mapM_ print $ linesFromDevice dev
