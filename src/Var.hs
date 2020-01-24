module Var where

import Data.Either (lefts, rights)
import Data.List.NonEmpty
import Data.Semigroup

import Syn

newtype Var a = Var (Event Internal [Either a (Event Internal a)])

newVar :: Semigroup a => Monoid v => a -> (Var a -> Syn v b) -> Syn v b
newVar a f = local' (<>) $ \e -> pool $ \p -> do
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

putVar :: Var a -> a -> Syn v ()
putVar (Var e) a = emit e [Left a]

-- TODO: order is swapped, Last = First
testVars :: Syn () (Last Int, Last Int, Last Int)
testVars = newVar (Last 5) $ \v -> do
  putVar v (Last 6)
  a <- readVar v
  _ <- andd' [putVar v (Last 8), putVar v (Last 9), putVar v (Last 10)]
  b <- readVar v
  (_, c) <- andd (putVar v (Last 11), readVar v)
  pure (a, b, c)
