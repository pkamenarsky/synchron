module Connector.HTTP where

import Concur

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad.Trans.Cont

import Network.Wai
import Network.Wai.Handler.Warp

newtype HTTP = HTTP (TChan (Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived))

http :: Port -> (HTTP -> IO ()) -> IO ()
http port k = do
  ch <- newTChanIO

  withAsync (go ch) $ \as -> do
    k (HTTP ch)
    cancel as

  where
    go ch = run port $ \req respond -> do
      f <- atomically $ readTChan ch
      f req respond

receive
  :: HTTP
  -> (Request -> (Response -> IO ResponseReceived) -> IO (ResponseReceived, a))
  -> Concur a
receive (HTTP app) f = do
  v <- step $ do
    v <- newEmptyTMVar

    writeTChan app $ \req respond -> do
      (r, a) <- f req respond
      atomically $ putTMVar v a
      pure r

    pure v

  step $ readTMVar v
