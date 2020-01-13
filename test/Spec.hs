import Data.Maybe (fromJust)
import Data.IORef

import Test.Tasty
import Test.Tasty.HUnit

import Syn hiding (emit, await)
import qualified Syn

localNid :: NodeId
localNid = NodeId 0

emit :: Event Internal a -> a -> Syn () ()
emit = Syn.emit

await :: Event t a -> Syn () a
await = Syn.await

p1 = local $ \e -> do
  a <- andd (await e, emit e "A", emit e "C")
  b <- orr [ Left <$> await e, Right <$> emit e "B" ]
  pure (a, b)

p2 = local $ \e -> do
  andd (emit e "E", await e)

p2_2 = local $ \e -> do
  andd (await e, emit e "E")

p2_3 = local $ \e -> do
  andd ((emit e "E" >> emit e "F"), ((,) <$> await e <*> await e))

p2_4 = local $ \e -> local $ \f -> do
  andd (andd (await e, await f), orr [ emit e 5, emit f 6 ])

p2_5 = local $ \e -> local $ \f -> do
  andd (orr [ await e, await f ], orr [ emit e 5, emit f 6 ])

p2_6 = local $ \e -> do
  orr [ Left <$> emit e "E", Right <$> await e ]

p2_7 = local $ \e -> do
  orr [ Right <$> await e, Left <$> emit e "E" ]

p3 = local $ \e -> local $ \f -> do
  a <- andd
         ( await e >> emit f "F"
         , await f
         , emit e "E"
         )
  pure a

p4 = local $ \e -> local $ \f -> do
  a <- andd
         ( andd (await e, emit f "F")
         , await f
         , andd (pure "_" :: Syn () String, await f >> emit e "E")
         )
  pure a

p5 = local $ \e -> do
  andd
    ( go 0 e
    , emit e (Right ())
    )
  where
    go :: Int -> Event Internal (Either Int ()) -> Syn () Int
    go s e = do
      a <- await e
      case a of
        Left n  -> go (s + n) e
        Right _ -> pure s

p6 = local $ \e -> local $ \f -> local $ \g -> do
  a <- andd
    ( andd (await e, emit f "F" >> await g >> emit e "E")
    , andd (await f, await g, await e)
    , andd (await e, await g, await f)
    , andd (pure "_" :: Syn () String, await f >> emit g "G")
    )
  pure a

p7 = local $ \e -> local $ \f -> do
  andd
    ( go 0 0 e f
    , do
        emit f 1
        emit f 2
        emit f 3
        emit f 6
        emit f 8
        emit e (Right ())
    )
  where
    go :: Int -> Int -> Event Internal (Either Int ()) -> Event Internal Int -> Syn () Int
    go x y e f = do
      a <- orr [ Left <$> await e, Right <$> await f ]
      case a of
        Left (Left x') -> go (x + x') y e f
        Right y'       -> go x (y + y') e f
        _              -> pure (x + y)

p8 = pool $ \p -> local $ \e -> do
  (a, _) <- andd (await e, emit e 5)
  (b, _) <- andd (await e, spawn p (emit e 5))

  pure (a + b)

p9 = local $ \e -> pool $ \p -> do
  spawn p (emit e 5)
  a <- await e

  spawn p (emit e 6)
  b <- await e

  spawn p (emit e 7)
  c <- await e

  pure (a + b + c)

p9_2 = local $ \e -> pool $ \p -> do
  spawn p (emit e 5)
  a <- await e

  spawn p (emit e 6 >> spawn p (emit e 7))
  b <- await e
  c <- await e

  pure (a + b + c)

p10 = local $ \i -> local $ \o -> pool $ \p -> do
  spawn p (go i o 0 3)

  andd (await o, emit i 1 >> spawn p (emit i 2 >> spawn p (emit i 3)))

  where
    go i o x 0 = emit o x
    go i o x n = do
      a <- await i
      go i o (x + a) (n - 1)

p11 = do
  (a, _, ks) <- fromJust $ runOrr $ mconcat
    [ liftOrr (pure "a")
    , liftOrr (pure "b")
    , liftOrr (pure "c")
    ]
  (b, _, ks') <- fromJust $ runOrr ks
  (c, _, ks'') <- fromJust $ runOrr ks'
  pure (a, b, c)

e12 result = event localNid $ \e -> do
  ctx <- run localNid $ do
    a <- andd' [ await e, await e ]
    async (result a)

  push ctx e "E"

  pure ctx

e13 result = event localNid $ \e -> do
  ctx <- run localNid $ local $ \f -> do
    a <- andd (await e, await e, await e, emit f "F")
    async (result a)

  push ctx e "E"

  pure ctx

e14 result = event localNid $ \e -> do
  ctx <- run localNid $ local $ \f -> do
    a <- andd (await e, await e, (,) <$> await f <*> await f, emit f "F" >> emit f "F")
    async (result a)

  push ctx e ("E","E")

  pure ctx

--------------------------------------------------------------------------------

test :: (Show a, Eq a) => Syn () a -> a -> Assertion
test f a = ((fromJust . fst) <$> exhaust localNid f) >>= (@?= a)

testE :: (Show a, Eq a) => ((a -> IO ()) -> IO (Context () b)) -> a -> Assertion
testE f a = do
  v <- newIORef Nothing
  f (writeIORef v . Just)
  b <- readIORef v
  b @?= Just a

main :: IO ()
main = defaultMain $ testGroup "Example tests"
  [ testCase "p1" $ test p1 (("C",(),()),Left "B")
  , testCase "p2" $ test p2 ((),"E")
  , testCase "p2_2" $ test p2_2 ("E",())
  , testCase "p2_3" $ test p2_3 ((),("E","F"))
  , testCase "p2_4" $ test p2_4 ((5,6),())
  , testCase "p2_5" $ test p2_5 (5,())
  , testCase "p2_6" $ test p2_6 (Left ())
  , testCase "p2_7" $ test p2_7 (Right "E")
  , testCase "p3" $ test p3 ((),"F",())
  , testCase "p4" $ test p4 (("E",()),"F",("_",()))
  , testCase "p5" $ test p5 (0,())
  , testCase "p6" $ test p6 (("E",()),("F","G","E"),("E","G","F"),("_",()))
  , testCase "p7" $ test p7 (20,())
  , testCase "p8" $ test p8 10
  , testCase "p9" $ test p9 18
  , testCase "p9_2" $ test p9_2 18
  , testCase "p10" $ test p10 (6,())
  , testCase "p11" $ test p11 ("a","b","c")

  , testCase "e12" $ testE e12 ["E", "E"]
  , testCase "e13" $ testE e13 ("E","E","E",())
  , testCase "e14" $ testE e14 (("E","E"),("E","E"),("F","F"),())
  ]
