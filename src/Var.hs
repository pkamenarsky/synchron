{-# LANGUAGE GADTs #-}

module Var where

import Data.Either (lefts, rights)
import Data.List.NonEmpty
import Data.Semigroup

import Syn

data Var a where
  Var :: (Event Internal [Either a (Event Internal a)]) -> Var a
  VPure :: a -> Var a
  VMap :: (a -> b) -> Var a -> Var b
  VApply :: Var (a -> b) -> Var a -> Var b

instance Functor Var where
  fmap = VMap

instance Applicative Var where
  pure = VPure
  (<*>) = VApply

var :: Semigroup a => Monoid v => a -> (Var a -> Syn v b) -> Syn v b
var a f = local' (<>) $ \e -> pool $ \p -> do
  spawn p (trail a e)
  f (Var e)
  where
    trail a e = do
      r <- await e

      let a' = case sconcat <$> nonEmpty (lefts r) of
                 Just a' -> a'
                 Nothing -> a
            
          es = case rights r of
                 [] -> [pure ()]
                 es -> fmap (flip emit a') es

      snd <$> andd (andd' es, trail a' e)

readVar :: Var a -> Syn v a
readVar (Var e) = local $ \c -> snd <$> andd (emit e [Right c], await c)
readVar (VPure a) = pure a
readVar (VMap f a) = f <$> readVar a
readVar (VApply f a) = (\(f', a') -> f' a') <$> andd (readVar f, readVar a)

putVar :: Var a -> a -> Syn v ()
putVar (Var e) a = emit e [Left a]

data Stream v a b = Stream { runStream :: a -> Syn v (Either b (Stream v a b)) }

stream :: (a -> Syn v (Either b (Stream v a b))) -> Stream v a b
stream = Stream

loop :: Semigroup a => Monoid v => Var a -> Stream v a b -> Syn v b
loop v@(Var e) f = do
  a <- readVar v
  go f a e
    
  where
    go f a e = do
      r <- orr [ Right <$> await e, Left <$> runStream f a ]
      case r of
        Left (Left a) -> pure a
        Left (Right f') -> go f' a e
        Right r -> do
          let a' = case sconcat <$> nonEmpty (lefts r) of
                     Just a' -> a'
                     Nothing -> a
          go f a' e

-- TODO: order is swapped, Last = First
testVars :: Syn () (Last Int, Last Int, Last Int)
testVars = var (Last 5) $ \v -> do
  putVar v (Last 6)
  a <- readVar v
  _ <- andd' [putVar v (Last 8), putVar v (Last 9), putVar v (Last 10)]
  b <- readVar v
  (_, c) <- andd (putVar v (Last 11), readVar v)
  pure (a, b, c)

--------------------------------------------------------------------------------

reflect :: Syn u x -> (u -> Syn v a) -> Syn v a
reflect = undefined

reflect' :: Syn u x -> Stream u v a -> Syn v a
reflect' = undefined

refract :: Event Internal u -> Syn (u, v) a -> Syn v a
refract e p = reflect p $ \(u, v) -> emit e u >> view v >> forever

reflrefr :: Monoid v => Syn (u, v) x -> (u -> Syn v x) -> Syn v x
reflrefr p f = reflect p $ \(u, v) -> orr [f u, view v >> forever]

data DOM

instance Semigroup DOM
instance Monoid DOM

inputWithState :: Syn (String, DOM) x
inputWithState = undefined

searchResults :: Event Internal String -> Syn [String] x
searchResults = undefined

searchResults' :: String -> Syn [String] x
searchResults' = undefined

ie = local $ \i -> orr
  [ refract i inputWithState
  , reflect (searchResults i) $ \results -> undefined -- results :: [String]
  ]
