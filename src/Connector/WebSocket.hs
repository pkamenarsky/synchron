{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Connector.WebSocket where

import qualified Syn

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

data WebSocketServer s = WebSocketServer (Syn.Event Syn.External WebSocket)

data WebSocket = WebSocket (Syn.Event Syn.External DataMessage) (TVar (TChan DataMessage))

websocket
  :: Monoid v
  => Port
  -> ConnectionOptions
  -> (forall s. WebSocketServer s -> IO (Syn.Context v a))
  -> IO (Syn.Context v a)
websocket port options k = Syn.event $ \e -> do
  ctx <- k (WebSocketServer e)
  forkIO (go ctx e)

  pure ctx

  where
    go ctx e = run port $ websocketsOr defaultConnectionOptions (wsApp ctx e) backupApp

    wsApp ctx e pending = do
      conn  <- acceptRequest pending

      inCh  <- Syn.newEvent
      outCh <- newTChanIO

      forkPingThread conn 30

      let inA = forever $ do
            r <- receiveDataMessage conn
            Syn.push ctx inCh r

          inB = forever $ do
            r <- atomically $ readTChan outCh
            sendDataMessage conn r

      wRef <- newTVarIO outCh
      Syn.push ctx e (WebSocket inCh wRef)

      void $ withAsync inA $ \a -> do
        withAsync inB $ \b -> do
          mkWeakTVar wRef $ do
            cancel a
            cancel b
          waitEitherCatchCancel a b

    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

accept :: WebSocketServer s -> Syn.Syn v WebSocket
accept (WebSocketServer e) = Syn.await e

receive :: WebSocket -> Syn.Syn v DataMessage
receive (WebSocket e _) = Syn.await e

send :: WebSocket -> DataMessage -> Syn.Syn v ()
send (WebSocket _ v) m = Syn.async $ atomically $ do
  outCh <- readTVar v
  writeTChan outCh m
