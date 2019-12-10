{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Concur where

import Control.Applicative
import Control.Monad.Fail (MonadFail (fail))
import Control.Monad.Free

import Control.Concurrent.STM

import Data.Maybe (isJust)

data ConcurF next
  = forall a. Step (STM a) (a -> next)
  | forall a. Orr [Concur a] ((a, [Concur a]) -> next)
  | forall a. Andd [Concur a] ([a] -> next)

deriving instance Functor ConcurF

newtype Concur a = Concur { getConcurF :: Free ConcurF a }
  deriving (Functor, Applicative, Monad)

instance MonadFail Concur where
  fail e = error e

instance Alternative Concur where
  empty = step empty
  a <|> b = orr [a, b]

step :: STM a -> Concur a
step io = Concur $ liftF (Step io id)

orr :: [Concur a] -> Concur a
orr ss = fmap fst $ Concur $ liftF (Orr ss id)

orr' :: [Concur a] -> Concur (a, [Concur a])
orr' ss = Concur $ liftF (Orr ss id)

andd :: [Concur a] -> Concur [a]
andd ss = Concur $ liftF (Andd ss id)

runStep :: Concur a -> STM (Either a (Concur a))
runStep (Concur (Pure a)) = pure (Left a)
runStep (Concur (Free (Step step next))) = do
  a <- step
  pure (Right $ Concur $ next a)
runStep (Concur (Free (Orr ss next))) = do
  (i, a) <- foldr (<|>) empty [ (i,) <$> runStep s | (s, i) <- zip ss [0..] ]
  case a of
    Left a   -> pure (Right $ Concur $ next (a, take i ss <> drop (i + 1) ss))
    Right s' -> pure (Right $ Concur $ Free $ Orr (take i ss <> [s'] <> drop (i + 1) ss) next)
runStep (Concur (Free (Andd ss next))) = do
  case traverse done ss of
    Just as -> pure (Right $ Concur $ next as)
    Nothing -> do
      (i, a) <- foldr (<|>) empty
        [ (i,) <$> runStep (if isJust (done s) then empty else s)
        | (s, i) <- zip ss [0..]
        ]
      case a of
        Left a'  -> pure (Right $ Concur $ Free $ Andd (take i ss <> [Concur $ Pure a'] <> drop (i + 1) ss) next)
        Right s' -> pure (Right $ Concur $ Free $ Andd (take i ss <> [s'] <> drop (i + 1) ss) next)
  where
    done (Concur (Pure a)) = Just a
    done _ = Nothing

runConcur :: Concur a -> IO a
runConcur s = do
  s' <- atomically $ runStep s
  case s' of
    Left a    -> pure a
    Right s'' -> runConcur s''

--------------------------------------------------------------------------------

newtype Pool s = Pool (TChan (Concur ()))

withPool :: (forall s. Pool s -> Concur a) -> Concur a
withPool k = do
  ch <- step newTChan
  go ch [ (Right . Left) <$> k (Pool ch) ]
  where
    go ch trails = do
      (a, ks) <- orr' trails
      case a of
        Left trail -> go ch $ concat
          [ [ Left <$> step (readTChan ch) ]
          , [ (Right . Right) <$> trail ]
          , ks
          ]
        Right (Left a)  -> pure a
        Right (Right _) -> go ch ks

spawn :: Pool s -> Concur () -> Concur ()
spawn (Pool ch) k = step (writeTChan ch k)

--------------------------------------------------------------------------------

newtype Signal a = Signal (TChan a)

newSignal :: Concur (Signal a)
newSignal = Signal <$> step newBroadcastTChan

emit :: Signal a -> a -> Concur ()
emit (Signal ch) a = step $ writeTChan ch a

await :: Signal a -> Concur a
await (Signal ch) = do
  ch' <- step $ dupTChan ch
  step $ readTChan ch'

