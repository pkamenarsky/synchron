module Var where

import Syn

newtype Var a = Var (Event Internal (Either a (Event Internal a)))

newVar :: a -> (Var a -> Syn v b) -> Syn v b
newVar a f = local $ \e -> go a e $ mconcat
  [ Left  <$> liftOrr (f (Var e))
  , Right <$> liftOrr (await e)
  ]
  where
    go a e ks = do
      (r, ks') <- unsafeRunOrr ks
      case r of
        Left b -> pure b
        Right (Left a') -> go a' e $ mconcat
          [ ks'
          , Right <$> liftOrr (await e)
          ]
        Right (Right c) -> go a e $ mconcat
          [ Orr $ fmap fst $ andd (unsafeRunOrr ks', emit c a)
          , Right <$> liftOrr (await e)
          ]

readVar :: Var a -> Syn v a
readVar (Var e) = local $ \c -> do
  emit e (Right c)
  await c

putVar :: Var a -> a -> Syn v ()
putVar (Var e) a = emit e (Left a)

testVars = newVar 5 $ \v -> do
  putVar v 6
  putVar v 7
  a <- readVar v
  _ <- andd' [putVar v 8, putVar v 9, putVar v 10]
  b <- readVar v
  pure (a, b)
