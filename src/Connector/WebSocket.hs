{-# LANGUAGE OverloadedStrings #-}

module Connector.WebSocket where

import Lib

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import Network.HTTP.Types

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets

import Network.WebSockets
import Network.WebSockets.Connection

data WebSocket = WebSocket (Chan DataMessage) (Chan DataMessage)

websocket :: Port -> ConnectionOptions -> (WebSocket -> Concur ()) -> IO ()
websocket port options k
  = run port $ websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp pending = do
      inCh  <- newChan
      outCh <- newChan

      conn  <- acceptRequest pending

      inTid <- forkIO $ forever $ do
        r <- receiveDataMessage conn
        writeChan inCh r

      outTid <- forkIO $ forever $ do
        r <- readChan outCh
        sendDataMessage conn r

      runConcur $ k $ WebSocket inCh outCh

      killThread inTid
      killThread outTid

    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

receive :: WebSocket -> Concur DataMessage
receive (WebSocket inCh _) = liftIO $ readChan inCh

send :: WebSocket -> DataMessage -> Concur ()
send (WebSocket _ outCh) m = liftIO $ writeChan outCh m
