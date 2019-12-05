{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Connector.WebSocket where

import Lib (Concur, runConcur, orr, andd, step)
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

testConcur = do
  c <- newTChanIO

  forkIO $ forever $ do
    a <- atomically $ readTChan c
    traceIO a

  v1 <- registerDelay 1000000
  v2 <- registerDelay 2000000
  v3 <- registerDelay 1500000
  v4 <- registerDelay 3000000
  v5 <- registerDelay 2500000

  (_, rs) <- runConcur $ do
    (_, rs) <- orr
      [ dp v3 c "V3"
      , dp v5 c "V5"
      , do
          (_, rs) <- orr [ dp v1 c "A", dp v2 c "B", dp v4 c "C" ]
          (_, rs) <- orr rs
          (_, rs) <- orr rs
          pure ()
      ]
    (_, rs) <- orr rs
    orr rs

  print $ length rs

  where
    dp v c s = do
      step $ writeTChan c ("BEFORE: " <> s)
      step $ do
        v' <- readTVar v
        check v'
      step $ writeTChan c ("AFTER: " <> s)

    f c n = do
      step $ writeTChan c (show n)
      f c (n + 1)

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

loopOrr :: [Concur a] -> (a -> Concur [Concur a]) -> Concur x
loopOrr st f = do
  (a, st') <- orr st
  st'' <- f a
  loopOrr (st' <> st'') f

test :: IO ()
test = do
  websocket 3922 defaultConnectionOptions $ \wss -> do
  websocket 3923 defaultConnectionOptions $ \wss2 -> do
  logger $ \log -> do
    runConcur $ server' log wss wss2

    where
      server' log wss wss2
        = loopOrr [ Left <$> andd [ accept wss, accept wss2 ] ] $ \r -> do
            case r of
              Left [ws, ws2] -> pure
                [ Left  <$> andd [ accept wss, accept wss2 ]  -- restart accept
                , Right <$> go log ws ws2                     -- add new connection
                ]
              Right _ -> pure []

      server log wss wss2 conns = do
        log ("LENGTH: " <> show (length conns))
        (r, ks) <- orr conns
        case r of
          Left [ws, ws2] -> server log wss wss2 $ concat
            [ [ Left  <$> andd [ accept wss, accept wss2 ] ]  -- restart accept
            , [ Right <$> go log ws ws2 ]                     -- add new connection
            , ks                                              -- keep rest of connections
            ]
          Right _        -> server log wss wss2 ks
        
        
      go log ws ws2 = do
        (r, _) <- orr
          [ fmap Left  <$> Connector.WebSocket.receive ws
          , fmap Right <$> Connector.WebSocket.receive ws2
          ]
        case r of
          Nothing  -> pure ()
          _  -> do
            log $ show r
            go log ws ws2
