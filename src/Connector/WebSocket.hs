{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Connector.WebSocket where

import Concur (Concur, runConcur, orr, andd, step)
import Connector.Log

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

data WebSocketServer s = WebSocketServer (TChan WebSocket)

newtype WebSocket = WebSocket (TVar (Maybe (TChan DataMessage, TChan DataMessage)))

websocket
  :: Port
  -> ConnectionOptions
  -> (forall s. WebSocketServer s -> IO ())
  -> IO ()
websocket port options k = do
  ch <- newTChanIO

  withAsync (go ch) $ \as -> do
    k $ WebSocketServer ch
    cancel as

  where
    go ch = run port $ websocketsOr defaultConnectionOptions (wsApp ch) backupApp

    wsApp ch pending = do
      conn  <- acceptRequest pending

      inCh  <- newTChanIO
      outCh <- newTChanIO

      forkPingThread conn 30

      let inA = forever $ do
            r <- receiveDataMessage conn
            atomically $ writeTChan inCh r

          inB = forever $ do
            r <- atomically $ readTChan outCh
            sendDataMessage conn r

      ws <- newTVarIO $ Just (inCh, outCh)
      atomically $ writeTChan ch (WebSocket ws)

      withAsync inA $ \a -> do
      withAsync inB $ \b -> do
        mkWeakTVar ws $ do
          cancel a
          cancel b
        waitEitherCatchCancel a b

      atomically $ writeTVar ws Nothing

    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

accept :: WebSocketServer s -> Concur WebSocket
accept (WebSocketServer ch) = step $ readTChan ch

receive :: WebSocket -> Concur (Maybe DataMessage)
receive (WebSocket v) = step $ do
  ws <- readTVar v

  case ws of
    Just (inCh, _) -> Just <$> readTChan inCh
    Nothing        -> pure Nothing

send :: WebSocket -> DataMessage -> Concur Bool
send (WebSocket v) m = step $ do
  ws <- readTVar v

  case ws of
    Just (_, outCh) -> do
      writeTChan outCh m
      pure True
    Nothing -> pure False
