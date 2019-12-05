module Connector.Log where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Control.Monad (forever)

import Lib

import Debug.Trace (traceIO)

logger :: ((String -> Concur ()) -> IO ()) -> IO ()
logger k = do
  ch <- newTChanIO

  withAsync (go ch) $ \as -> do
    k (step . writeTChan ch)
    cancel as

  where
    go ch = forever $ do
      t <- atomically $ readTChan ch
      traceIO t
