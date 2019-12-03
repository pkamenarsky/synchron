{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Connector.WebSocket where

import Lib (Concur, runConcur, orr, andd)

import BroadcastChan
import qualified BroadcastChan.Throw as BT

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.Free
import Control.Monad.IO.Class

import Data.IORef

import Network.HTTP.Types

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets

import Network.WebSockets
import Network.WebSockets.Connection

import Debug.Trace

data WebSocketServer s = WebSocketServer (BroadcastChan In DataMessage) (Chan DataMessage)

data WebSocket = WebSocket (BroadcastChan Out DataMessage) (Chan DataMessage)

websocket
  :: Port
  -> ConnectionOptions
  -> (forall s. WebSocketServer s -> IO ())
  -> IO ()
websocket port options k = do
  run port $ websocketsOr defaultConnectionOptions wsApp backupApp
  pure undefined
  where
    wsApp pending = do
      inCh  <- newBroadcastChan
      outCh <- newChan

      conn  <- acceptRequest pending

      inTid <- forkIO $ forever $ do
        r <- receiveDataMessage conn
        writeBChan inCh r

      outTid <- forkIO $ forever $ do
        r <- readChan outCh
        sendDataMessage conn r

      k $ WebSocketServer inCh outCh

      killThread inTid
      killThread outTid

    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

accept :: WebSocketServer s -> Concur WebSocket
accept (WebSocketServer inBCh outCh) = liftIO $ do
  inBChL <- newBChanListener inBCh
  pure $ WebSocket inBChL outCh

accept' :: WebSocketServer s -> (WebSocket -> Concur ()) -> Concur ()
accept' = undefined

receive :: WebSocket -> Concur DataMessage
receive (WebSocket inBChL _) = liftIO $ BT.readBChan inBChL

send :: WebSocket -> DataMessage -> Concur ()
send (WebSocket _ outCh) m = liftIO $ writeChan outCh m

test :: IO ()
test = do
  websocket 6666 defaultConnectionOptions $ \wss -> do
  websocket 6667 defaultConnectionOptions $ \wss2 -> do
    runConcur (acceptBoth wss wss2 Nothing Nothing)

    where
      acceptBoth wss wss2 ws' ws2' = case (ws', ws2') of
        (Just ws, Just ws2) -> go ws ws2
        (Nothing, Just ws2) -> do
          ws <- accept wss
          go ws ws2
        (Just ws, Nothing)  -> do
          ws2 <- accept wss2
          go ws ws2
        (Nothing, Nothing)  -> do
          r <- orr [ Left <$> accept wss, Right <$> accept wss2 ]
          case r of
            Left  ws  -> acceptBoth wss wss2 (Just ws) ws2'
            Right ws2 -> acceptBoth wss wss2 ws' (Just ws2)

      acceptBoth' wss wss2 _ _ = do
        [ws, ws2] <- andd [ accept wss, accept wss2 ]
        go ws ws2

      go ws ws2 = do
        r <- orr
          [ Left  <$> Connector.WebSocket.receive ws
          , Right <$> Connector.WebSocket.receive ws2
          ]
        liftIO $ case r of
          Left  ds -> print ds
          Right ds -> print ds
        go ws ws2

--------------------------------------------------------------------------------

data Mu f = Mu (f (Mu f)) | Done

runC :: Mu STM -> IO ()
runC = undefined

acceptSTM :: Concur WebSocket
acceptSTM = undefined

receiveSTM :: WebSocket -> STM DataMessage
receiveSTM = undefined

sendSTM :: WebSocket -> DataMessage -> STM ()
sendSTM = undefined

liftSTM :: STM a -> Concur a
liftSTM = undefined

orrSTM :: [STM a] -> STM a
orrSTM = undefined

testa = go
  where
    go = do
      a <- orr [ acceptSTM, acceptSTM ]

      liftSTM $ do
        b <- orrSTM [ receiveSTM a, receiveSTM a ]
        sendSTM a b

      go

--------------------------------------------------------------------------------

data S a = forall s. S {unS :: s -> IO (s, a) }

instance Functor S where
  fmap f (S m) = S $ \s -> do
    (s', a) <- m s
    pure (s', f a)

instance Applicative S

instance Monad S where
  return a = S $ \s -> pure (s, a)
  S m >>= f = S $ \s -> do
    (s', a) <- m s
    -- let S m' = f a
    undefined

--------------------------------------------------------------------------------

data Resumable
data Interruptible

type Continuation a = TVar (Maybe (Suspend Resumable a))

newCont :: IO (Continuation a)
newCont = newTVarIO Nothing

data SuspendF next
  = forall a. Step (STM a) (a -> next)
  | forall a. Resume (Continuation a) (Suspend Interruptible a)
  | Break next

deriving instance Functor SuspendF

newtype Suspend t a = Suspend (Free SuspendF a)
  deriving (Functor, Applicative, Monad)

step :: STM a -> Suspend t a
step io = Suspend $ liftF (Step io id)

resume :: Continuation a -> Suspend Interruptible a -> Suspend Resumable a
resume k m = Suspend $ liftF (Resume k m)

break :: Suspend t ()
break = Suspend $ liftF (Break ())

runSuspend :: Maybe (Continuation a) -> Suspend t a -> IO ()
runSuspend _ (Suspend (Pure a)) = pure ()
runSuspend k (Suspend (Free (Step step next))) = do
  a <- atomically $ case k of
    Just k' -> do
      a <- step
      writeTVar k' (Just $ Suspend $ next a)
      pure a
    Nothing -> step
  runSuspend k (Suspend $ next a)
runSuspend (Just _) (Suspend (Free (Resume _ _))) = error "Nested resume"
runSuspend Nothing (Suspend (Free (Resume k t))) = do
  k' <- atomically $ readTVar k
  case k' of
    Just k'' -> runSuspend (Just k) k''
    Nothing  -> runSuspend (Just k) t
runSuspend k (Suspend (Free (Break next))) = do
  case k of
    Just k' -> atomically $ writeTVar k' (Just $ Suspend next)
    Nothing -> pure ()
  pure ()

testCont = do
  k <- newCont
  c <- newTChanIO

  forkIO $ forever $ do
    a <- atomically $ readTChan c
    traceIO a

  runSuspend Nothing $ resume k $ f c 0
  runSuspend Nothing $ resume k $ f c 0
  runSuspend Nothing $ resume k $ f c 0
  runSuspend Nothing $ resume k $ f c 0

  where
    f c n = do
      step $ writeTChan c (show n)
      Connector.WebSocket.break
      f c (n + 1)
