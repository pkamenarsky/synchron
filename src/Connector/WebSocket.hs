{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Connector.WebSocket where

import Lib (Concur, runConcur, orr, andd)

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Free
import Control.Monad.IO.Class

import Data.IORef
import Data.Maybe (fromMaybe)

import Network.HTTP.Types

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets

import Network.WebSockets
import Network.WebSockets.Connection

import Debug.Trace

data SuspendF next
  = forall a. Step (STM a) (a -> next)
  | forall a. Orr [Suspend a] ((a, [Suspend a]) -> next)
  | forall a. Andd [Suspend a] ([a] -> next)

deriving instance Functor SuspendF

newtype Suspend a = Suspend { getSuspendF :: Free SuspendF a }
  deriving (Functor, Applicative, Monad)

instance MonadFail Suspend where
  fail e = error e

step :: STM a -> Suspend a
step io = Suspend $ liftF (Step io id)

orrSuspend :: [Suspend a] -> Suspend (a, [Suspend a])
orrSuspend ss = Suspend $ liftF (Orr ss id)

anddSuspend :: [Suspend a] -> Suspend [a]
anddSuspend ss = Suspend $ liftF (Andd ss id)

runSuspend :: (Suspend a -> STM ()) -> Suspend a -> IO a
runSuspend retain (Suspend (Pure a)) = do
  atomically $ retain $ Suspend (Pure a)
  pure a
runSuspend retain (Suspend (Free (Step step next))) = do
  a <- atomically $ do
    a <- step
    retain (Suspend $ next a)
    pure a
  runSuspend retain (Suspend $ next a)
runSuspend retain (Suspend (Free (Orr ss next))) = do
  r <- newEmptyMVar
  k <- newTVarIO ss

  go k r (zip ss [0..]) []
  -- tids <- flip traverse (zip ss [0..]) $ \(s, i) -> forkIO $ do
  --   a <- flip runSuspend s $ \n -> do
  --     ks <- readTVar k
  --     let ks' = (take i ks <> [n] <> drop (i + 1) ks)
  --     writeTVar k ks'
  --     retain $ Suspend $ Free $ Orr ks' next
  --   putMVar r (a, i)

  -- (a, i) <- takeMVar r
  
  -- traverse killThread tids
  -- ks <- atomically $ readTVar k

  -- runSuspend retain $ Suspend $ next (a, (take i ks <> drop (i + 1) ks))
  where
    go k r [] as = do
      (a, i) <- takeMVar r
      traverse uninterruptibleCancel as
      ks <- atomically $ readTVar k
      runSuspend retain $ Suspend $ next (a, (take i ks <> drop (i + 1) ks))
    go k r ((s, i):xs) as = withAsync f $ \a -> go k r xs (a:as)
      where
        f = do
          a <- flip runSuspend s $ \n -> do
            ks <- readTVar k
            let ks' = (take i ks <> [n] <> drop (i + 1) ks)
            writeTVar k ks'
            retain $ Suspend $ Free $ Orr ks' next
          putMVar r (a, i)

runSuspend retain (Suspend (Free (Andd ss next))) = do
  rs <- traverse (const newEmptyMVar) ss
  k  <- newTVarIO ss

  tids <- flip traverse (zip3 rs ss [0..]) $ \(r, s, i) -> forkIO $ do
    a <- flip runSuspend s $ \n -> do
      ks <- readTVar k
      let ks' = (take i ks <> [n] <> drop (i + 1) ks)
      writeTVar k ks'
      retain $ Suspend $ Free (Andd ks' next)
    putMVar r a

  as <- traverse takeMVar rs

  runSuspend retain $ Suspend (next as)

testCont = do
  c <- newTChanIO

  forkIO $ forever $ do
    a <- atomically $ readTChan c
    traceIO a

  v1 <- registerDelay 1000000
  v2 <- registerDelay 2000000
  v3 <- registerDelay 1500000

  (_, rs) <- runSuspend (const $ pure ()) $ do
    (_, rs) <- orrSuspend
      [ dp v3 c "V3"
      , do
          (_, rs) <- orrSuspend [ dp v1 c "A", dp v2 c "B" ]
          (_, rs) <- orrSuspend rs
          pure ()
      ]
    orrSuspend rs

  print $ length rs

  -- runSuspend Nothing $ resume k $ f c 0
  -- runSuspend Nothing $ resume k $ f c 0
  -- runSuspend Nothing $ resume k $ f c 0
  -- runSuspend Nothing $ resume k $ f c 0

  pure ()

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
  ch     <- newTChanIO
  server <- async $ run port $ websocketsOr defaultConnectionOptions (wsApp ch) backupApp

  k $ WebSocketServer ch
  cancel server

  where
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
        waitEither a b

      atomically $ writeTVar ws Nothing

    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

accept :: WebSocketServer s -> Suspend WebSocket
accept (WebSocketServer ch) = step $ readTChan ch

receive :: WebSocket -> Suspend (Maybe DataMessage)
receive (WebSocket v) = step $ do
  ws <- readTVar v

  case ws of
    Just (inCh, _) -> Just <$> readTChan inCh
    Nothing        -> pure Nothing

send :: WebSocket -> DataMessage -> Suspend Bool
send (WebSocket v) m = step $ do
  ws <- readTVar v

  case ws of
    Just (_, outCh) -> do
      writeTChan outCh m
      pure True
    Nothing -> pure False

test :: IO ()
test = do
  websocket 6666 defaultConnectionOptions $ \wss -> do
  websocket 6667 defaultConnectionOptions $ \wss2 -> do
    runSuspend (const $ pure ()) $ do
      [ws, ws2] <- anddSuspend [ accept wss, accept wss2 ]
      go ws ws2

    where
      go ws ws2 = do
        r <- orrSuspend
          [ Left  <$> Connector.WebSocket.receive ws
          , Right <$> Connector.WebSocket.receive ws2
          ]
        -- liftIO $ case r of
        --   Left  ds -> print ds
        --   Right ds -> print ds
        go ws ws2
