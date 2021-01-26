module Producers
    ( getDevice
    , toPipes
    , streamDevice
    , streamDeviceWith
    ) where

import Evdev
import Stream

import Streamly
import qualified Streamly.Prelude as S

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import qualified Pipes as P
import qualified Pipes.Prelude as P

toPipes :: Monad m => SerialT m a -> P.Producer a m ()
toPipes = P.unfoldr unconsS
  where
    -- Adapt S.uncons to return an Either instead of Maybe
    unconsS s = S.uncons s >>= (return . maybe (Left ()) Right)

getDevice :: String -> IO Device
getDevice name = do
  dev <- newDevice . encodeUtf8 $ T.pack name
  grabDevice dev
  return dev

streamDevice :: Device -> SerialT IO String
streamDevice = linesFromDevice

streamDeviceWith :: (String -> a) -> Device -> SerialT IO a
streamDeviceWith f = S.map f . streamDevice
