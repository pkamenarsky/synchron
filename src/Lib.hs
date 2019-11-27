{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( someFunc
    ) where

import Control.Applicative
import Control.Monad
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

runConcur :: Concur () -> IO ()
runConcur (Concur c) = runContT c pure

orr :: [Concur a] -> Concur a
orr [c] = c
orr cs = Concur $ ContT $ \k -> do
  r <- newEmptyMVar
  rs <- forM cs $ \(Concur c) -> do
    async $ runContT c k
  void $ waitAnyCancel rs

someFunc :: IO ()
someFunc = runConcur $ do
  a <- orr [ Left <$> delay 1000000 "asd", Right <$> delay 1000000 "cde" ]
  liftIO $ traceIO $ show a
  where
    delay n a = do
      liftIO $ traceIO "Before"
      liftIO $ threadDelay n
      liftIO $ traceIO "After"
      pure a
