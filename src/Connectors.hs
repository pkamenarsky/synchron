module Connectors where

import Lib

import Control.Concurrent
import Control.Monad.Trans.Cont

import Network.Wai
import Network.Wai.Handler.Warp

newtype HTTP = HTTP (Chan (Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived))

http :: Port -> IO HTTP
http port = do
  ch <- newChan
  run port $ \req respond -> do
    f <- readChan ch
    f req respond
  pure $ HTTP ch

receive
  :: HTTP
  -> (Request -> (Response -> IO ResponseReceived) -> IO (ResponseReceived, a))
  -> Concur a
receive (HTTP app) f = Concur $ ContT $ \k -> do
  writeChan app $ \req respond -> do
    (r, a) <- f req respond
    k a
    pure r
