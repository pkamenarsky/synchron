module Connector.Log where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Control.Monad (forever)

import Concur

import Debug.Trace (traceIO)

logger :: ((String -> Concur ()) -> IO ()) -> IO ()
logger k = do
  ch   <- newTChanIO
  done <- newEmptyTMVarIO

  withAsync (go ch done) $ \as -> do
    k (step . writeTChan ch)
    atomically $ putTMVar done ()
    wait as

  where
    go ch done = do
      r <- atomically $ fmap Left (readTChan ch) `orElse` fmap Right (readTMVar done)
      case r of
        Left r' -> do
          traceIO r'
          go ch done
        Right _ -> pure ()
