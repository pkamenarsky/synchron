import Test.Tasty
import Test.Tasty.HUnit

import RSP

p1 = global $ \e -> run $ do
  a <- andd [ Left <$> ((,) <$> await e <*> await e), Right <$> emit e "A", Right <$> emit e "C" ]
  b <- orr [ Left <$> await e, Right <$> emit e "B" ]
  pure (a, b)

p2 = global $ \e -> run $ do
  a <- andd [ Left <$> emit e "E", Right <$> await e ]
  pure a

p3 = global $ \e -> global $ \f -> run $ do
  a <- andd
    [ Left  <$> (await e >> emit f "F")
    , Right <$> await f
    , Left  <$> emit e "E"
    ]
  pure a

p4 = global $ \e -> global $ \f -> run $do
  a <- andd
    [ Left  <$> andd [ Left <$> await e, Right <$> emit f "F" ]
    , Right <$> await f
    , Left  <$> andd [ Left <$> pure "_", Right <$> (await f >> emit e "E") ]
    ]
  pure a

p5 = global $ \e -> run $ do
  andd
    [ Left  <$> go 0 e
    , Right <$> do
        emit e (Left 1)
        emit e (Left 2)
        emit e (Left 3)
        emit e (Left 4)
        emit e (Right ())
    ]
  where
    go :: Int -> Event (Either Int ()) -> RSP Int
    go s e = do
      a <- await e
      case a of
        Left n  -> go (s + n) e
        Right _ -> pure s

p6 = run $ local $ \e -> local $ \f -> local $ \g -> do
  a <- andd
    [ Left  <$> andd [ Left <$> await e, Right <$> (emit f "F" >> await g >> emit e "E") ]
    , Right <$> andd [ await f, await g, await e ]
    , Right <$> andd [ await e, await g, await f ]
    , Left  <$> andd [ Left <$> pure "_", Right <$> (await f >> emit g "G") ]
    ]
  pure a

p7 = global $ \e -> global $ \f -> run $ do
  andd
    [ Left  <$> go 0 0 e f
    , Right <$> do
        emit e (Left 1)
        emit e (Left 2)
        emit e (Left 3)
        emit e (Left 4)
        emit f 9
        emit f 1
        emit e (Right ())
    ]
  where
    go :: Int -> Int -> Event (Either Int ()) -> Event Int -> RSP Int
    go x y e f = do
      a <- orr [ Left <$> await e, Right <$> await f ]
      case a of
        Left (Left x') -> go (x + x') y e f
        Right y'       -> go x (y + y') e f
        _              -> pure (x + y)

p8 = run $ pool $ \p -> local $ \e -> do
  [Left a, _] <- andd [ Left <$> await e, Right <$> spawn p (emit e 5) ]
  [Left b, _] <- andd [ Left <$> await e, Right <$> spawn p (emit e 5) ]

  pure (a + b)

p9_4 = run $ local $ \e -> pool $ \p -> do
  emit e 5
  async (print "4")
  await e

p9_5 = run $ local $ \e -> pool $ \p -> do
  spawn p (emit e 5)
  a <- await e
  async (print a)

  spawn p (emit e 6)
  b <- await e
  async (print b)

  spawn p (emit e 7)
  c <- await e
  async (print c)

  pure (a + b + c)

p9 = run $ local $ \e -> pool $ \p -> do
  spawn p (emit e 5)
  a <- await e
  async (print a)

  spawn p (emit e 6 >> spawn p (emit e 7))
  b <- await e
  async (print b)

  c <- await e
  async (print c)

  pure (a + b + c)

p10 = run $ local $ \i -> local $ \o -> pool $ \p -> do
  spawn p (go i o 2)

  andd [ Left <$> await o, Right <$> (emit i () >> spawn p (emit i () >> spawn p (emit i ()))) ]

  where
    go i o 0 = emit o 12
    go i o n = do
      async (print (show n))
      _ <- await i
      go i o (n - 1)

orr1 = run $ local $ \e -> do
  [_, Right (_, ks)] <- andd [ Left <$> await e, Right <$> orr' [ emit e (), emit e (), emit e () ] ]
  andd $ concat [ [ await e ], ks ]

orr2 = run $ local $ \e -> orr' [pure"a", await e, await e, await e, pure "orr"]

--------------------------------------------------------------------------------

test :: (Show a, Eq a) => IO a -> a -> Assertion
test f a = f >>= (@?= a)

main :: IO ()
main = defaultMain $ testGroup "Unit tests"
  [ testCase "p1" $ test p1 ([Left ("A","C"),Right (),Right ()],Left "B")
  , testCase "p2" $ test p2 [Left (),Right "E"]
  , testCase "p3" $ test p3 [Left (),Right "F",Left ()]
  , testCase "p4" $ test p4 [Left [Left "E",Right ()],Right "F",Left [Left "_",Right ()]]
  , testCase "p5" $ test p5 [Left 10,Right ()]
  , testCase "p6" $ test p6 [Left [Left "E",Right ()],Right ["F","G","E"],Right ["E","G","F"],Left [Left "_",Right ()]]
  , testCase "p7" $ test p7 [Left 20,Right ()]
  , testCase "p8" $ test p8 10
  ]
