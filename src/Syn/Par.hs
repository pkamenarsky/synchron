{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Syn.Par where

import Control.Monad.Free
import Control.Concurrent
import qualified Data.Map as M

import Data.IORef
import qualified Data.HashTable.IO as H

import Unsafe.Coerce (unsafeCoerce)

data EventId = Int
data Event a = Event EventId
data EventValue = forall a. Monoid a => EventValue (Event a) a

data SynF v next
  = Async (IO ()) next

  | Forever

  | View v next

  | forall u a. Monoid u => MapView (u -> v) (Syn u a) (a -> next)

  | forall a b. Monoid a => Local (Event a -> Syn v b) (b -> next)
  | Emit EventValue next
  | forall t a. Await (Event a) (a -> next)

  | forall a. Dyn (Event [Syn v ()]) (Syn v a) (a -> next)

  | forall a. Or [Syn v a] (a -> next)
  | forall a. And [Syn v a] ([a] -> next)

deriving instance Functor (SynF v)

newtype Syn v a = Syn { getSyn :: Free (SynF v) a }
  deriving (Functor, Applicative, Monad)

--------------------------------------------------------------------------------

type Frame = Int

data FrameTable = FrameTable
  { ftSize :: IORef Int
  , ftAck :: IORef Int
  , ftTable :: H.BasicHashTable EventId EventValue
  }

type EventTable = H.BasicHashTable Frame FrameTable

expect :: EventTable -> Frame -> Int -> IO ()
expect t frame add = H.mutateIO t frame $ \v -> case v of
  Nothing -> do
    fts <- newIORef add
    fta <- newIORef 0
    ftt <- H.new
    pure (Just $ FrameTable fts fta ftt, ())
  Just ft@(FrameTable fts _ _) -> do
    atomicModifyIORef' fts $ \s -> (s + add, ())
    pure (Just ft, ())

ack :: EventTable -> Frame -> IO ()
ack t frame = H.mutateIO t frame $ \v -> case v of
  Nothing -> error "ack"
  Just (FrameTable fts fta ftt)  -> do
    undefined

emit :: EventTable -> Frame -> Event a -> a -> IO ()
emit t frame e a = undefined

await :: EventTable -> Frame -> Event a -> IO a
await = undefined

--------------------------------------------------------------------------------

newtype Answer (r :: * -> *) = Answer (MVar (), MVar ())

newAnswer :: (Answer r -> IO ()) -> IO ()
newAnswer f = do
  va <- newEmptyMVar
  vb <- newEmptyMVar
  f (Answer (va, vb))

request :: Answer r -> r a -> IO a
request (Answer (va, vb)) a = do
  putMVar va (unsafeCoerce a)
  unsafeCoerce $ takeMVar vb

answer :: Answer r -> (r a -> IO a) -> IO ()
answer (Answer (va, vb)) f = do
  a <- takeMVar va
  b <- f (unsafeCoerce a)
  putMVar vb (unsafeCoerce b)

data R a where
  REmit :: EventValue -> R ()
  RAwait :: Event a -> R a

data ST v = ST (Answer R)

run :: ST v -> Syn v a -> IO a
run st (Syn (Pure a)) = pure a
run st (Syn (Free (Async io next))) = do
  io
  run st (Syn next)
run st (Syn (Free Forever)) = threadDelay maxBound >>= undefined
run st (Syn (Free (View v next))) = do
  run st (Syn next)
run st@(ST r) (Syn (Free (Await e next))) = do
  a <- request r (RAwait e)
  run st $ Syn (next a)
run st@(ST r) (Syn (Free (Emit ev next))) = do
  _ <- request r (REmit ev)
  undefined

