{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail (MonadFail (fail))
import qualified Control.Monad.Fail as F
import Control.Monad.Free
import Control.Monad.IO.Class

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Control.Monad.Trans.Cont

import Debug.Trace

newtype Concur a = Concur (ContT () IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance Alternative Concur where
  empty = Concur $ ContT $ const $ pure ()
  a <|> b = orr [a, b]

instance MonadFail Concur where
  fail e = Concur $ ContT $ const $ F.fail e

runConcur :: Concur () -> IO ()
runConcur (Concur c) = runContT c pure

orr :: [Concur a] -> Concur a
orr [c] = c
orr cs = Concur $ ContT $ \k -> do
  var  <- newEmptyMVar
  tids <- forM cs $ \(Concur c) -> forkIO $ runContT c (putMVar var)

  withMVar var $ \r -> do
    forM_ tids killThread
    k r

receive :: Chan a -> Concur a
receive = liftIO . readChan

send :: Chan a -> a -> Concur ()
send ch a = liftIO $ writeChan ch a

someFunc :: IO ()
someFunc = runConcur $ do
  a <- orr [ Left <$> delay 1500000 "asd", Right <$> delay2 1000000 "cde" ]
  liftIO $ traceIO $ show a
  where
    delay n a = do
      liftIO $ traceIO "Before"
      liftIO $ threadDelay n
      liftIO $ traceIO "After"
      pure a

    delay2 n a = do
      liftIO $ traceIO "Before2 1"
      liftIO $ threadDelay n
      liftIO $ traceIO "After2 1"
      liftIO $ traceIO "Before2 2"
      liftIO $ threadDelay n
      liftIO $ traceIO "After2 2"
      pure a
