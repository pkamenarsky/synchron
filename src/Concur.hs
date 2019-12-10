{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Concur where

import Control.Applicative
import Control.Monad (forever)
import Control.Monad.Fail (MonadFail (fail))
import Control.Monad.Free

import Control.Concurrent.Async
import Control.Concurrent.STM

import Data.Maybe (isJust)

data Pool s = Pool (TChan (Concur ()))

data ConcurF next
  = forall a. Step (STM a) (a -> next)
  | forall a. Orr [Concur a] (a -> next)
  | forall a. Andd [Concur a] ([a] -> next)
  | forall a. WithPool (forall s. Pool s -> Concur a) (a -> next)

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
orr ss = Concur $ liftF (Orr ss id)

andd :: [Concur a] -> Concur [a]
andd ss = Concur $ liftF (Andd ss id)

withPool :: (forall s. Pool s -> Concur a) -> Concur a
withPool k = Concur $ liftF (WithPool k id)

runStep :: Concur a -> IO (Either a (Concur a))
runStep (Concur (Pure a)) = pure (Left a)
runStep (Concur (Free (Step step next))) = do
  a <- atomically step
  pure (Right $ Concur $ next a)
runStep (Concur (Free (Orr ss next))) = do
  (i, a) <- foldr (<|>) empty [ (i,) <$> runStep s | (s, i) <- zip ss [0..] ]
  case a of
    Left a   -> pure (Right $ Concur $ next a)
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
runStep (Concur (Free (WithPool k next))) = do
  ch <- newTChanIO
  as <- async $ forever $ do
    trail <- atomically $ readTChan ch
    undefined
  a  <- runConcur (k $ Pool ch)
  cancel as
  pure (Right $ Concur $ next a)

runConcur :: Concur a -> IO a
runConcur s = do
  s' <- runStep s
  case s' of
    Left a    -> pure a
    Right s'' -> runConcur s''

--------------------------------------------------------------------------------

spawn :: Pool s -> Concur () -> Concur ()
spawn (Pool ch) k = step $ writeTChan ch k
