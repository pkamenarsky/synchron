{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Connector.WebSocket where

import qualified RSP

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Free
import Control.Monad.IO.Class

import Data.IORef
import Data.Maybe (catMaybes, isJust, fromMaybe)

import Network.HTTP.Types

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets

import Network.WebSockets
import Network.WebSockets.Connection

import Debug.Trace

data WebSocketServer s = WebSocketServer (RSP.Event RSP.External WebSocket)

data WebSocket = WebSocket (RSP.Event RSP.External DataMessage) (TVar (TChan DataMessage))

websocket
  :: Port
  -> ConnectionOptions
  -> (forall s. WebSocketServer s -> IO (RSP.Context a))
  -> IO (RSP.Context a)
websocket port options k = RSP.event $ \e -> do
  ctx <- k (WebSocketServer e)
  forkIO (go ctx e)

  pure ctx

  where
    go ctx e = run port $ websocketsOr defaultConnectionOptions (wsApp ctx e) backupApp

    wsApp ctx e pending = do
      conn  <- acceptRequest pending

      inCh  <- RSP.newEvent
      outCh <- newTChanIO

      forkPingThread conn 30

      let inA = forever $ do
            r <- receiveDataMessage conn
            RSP.push ctx inCh r

          inB = forever $ do
            r <- atomically $ readTChan outCh
            sendDataMessage conn r

      wRef <- newTVarIO outCh
      RSP.push ctx e (WebSocket inCh wRef)

      void $ withAsync inA $ \a -> do
        withAsync inB $ \b -> do
          mkWeakTVar wRef $ do
            cancel a
            cancel b
          waitEitherCatchCancel a b

    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

accept :: WebSocketServer s -> RSP.RSP WebSocket
accept (WebSocketServer e) = RSP.await e

receive :: WebSocket -> RSP.RSP DataMessage
receive (WebSocket e _) = RSP.await e

send :: WebSocket -> DataMessage -> RSP.RSP ()
send (WebSocket _ v) m = RSP.async $ atomically $ do
  outCh <- readTVar v
  writeTChan outCh m
