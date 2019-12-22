{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module RSP where

import Control.Concurrent
import Control.Monad.Free
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.State as ST
import qualified Control.Lens as L

import Data.IORef
import Data.List (intercalate)
import Data.Maybe (listToMaybe, mapMaybe)

import Unsafe.Coerce (unsafeCoerce)

import Debug.Trace

--------------------------------------------------------------------------------

type EventId = IORef ()

data Event a = Event EventId

type IsHole = Bool

data RSPF next
  = Async (IO ()) next

  | Forever

  | forall a b. Local (Event a -> RSP b) (b -> next)
  | Emit ExEvent next
  | forall a. Await (Event a) (a -> next)

  | forall a. Or (RSP a) (RSP a) (a -> next)
  | forall a. And (RSP a) (RSP a) ([a] -> next)

  | forall a. OrU (RSP a) (RSP a) (a -> next)
  | forall a. AndU (RSP a) (RSP a) ([a] -> next)

  | forall a. Canrun Int next

deriving instance Functor RSPF

newtype RSP a = RSP { getRSP :: Free RSPF a }
  deriving (Functor, Applicative, Monad)

instance Show (RSP a) where
  show (RSP (Pure a)) = "Pure"
  show (RSP (Free (Async _ _))) = "Async"
  show (RSP (Free Forever)) = "Forever"
  show (RSP (Free (Local _ _))) = "Local"
  show (RSP (Free (Emit _ _))) = "Emit"
  show (RSP (Free (Await _ _))) = "Await"
  show (RSP (Free (Or a b _))) = "Or [" <> intercalate " " (map show [a, b]) <> "]"
  show (RSP (Free (And a b _))) = "And [" <> intercalate " " (map show [a, b]) <> "]"
  show (RSP (Free (OrU a b _))) = "OrU [" <> intercalate " " (map show [a, b]) <> "]"
  show (RSP (Free (AndU a b _))) = "AndU [" <> intercalate " " (map show [a, b]) <> "]"
  show (RSP (Free (Canrun n p))) = "Canrun " <> show n

async :: IO () -> RSP ()
async io = RSP $ liftF (Async io ())

local :: (Event a -> IO b) -> IO b
local f = do
  e <- newIORef ()
  f (Event e)

emit :: Event a -> a -> RSP ()
emit e a = RSP $ liftF (Emit (ExEvent e a) ())

await :: Event a -> RSP a
await e = RSP $ liftF (Await e id)

orr' :: [RSP a] -> RSP (a, [RSP a])
orr' rsps = undefined

orr :: [RSP a] -> RSP a
orr [a] = a
orr [a, b] = RSP $ liftF (Or a b id)
orr (a:as) = orr [a, orr as]

andd :: [RSP a] -> RSP [a]
andd [a] = (:[]) <$> a
andd [a, b] = RSP $ liftF (And a b id)
andd (a:as) = concat <$> andd [(:[]) <$> a, andd as]

forever :: RSP a
forever = RSP $ liftF Forever

--------------------------------------------------------------------------------

data ExEvent = forall a. ExEvent (Event a) a

bcast :: ExEvent -> RSP a -> RSP a
bcast (ExEvent (Event e) a) (RSP (Free (Await (Event e') next)))
  | e == e' = RSP (next $ unsafeCoerce a)
bcast e (RSP (Free (AndU p q next)))
  = RSP (Free (AndU (bcast e p) (bcast e q) next))
bcast e (RSP (Free (OrU p q next)))
  = RSP (Free (OrU (bcast e p) (bcast e q) next))
bcast e (RSP (Free (Canrun n next)))
  = RSP (Free (Canrun n (getRSP $ bcast e (RSP next))))
bcast _ p = p

isBlocked :: RSP a -> Int -> Bool
isBlocked (RSP (Pure _)) _ = True
isBlocked (RSP (Free (Await _ _))) _ = True
isBlocked (RSP (Free (Canrun m _))) n = n > m
isBlocked (RSP (Free (And p q _))) n = isBlocked p n && isBlocked q n
isBlocked (RSP (Free (Or p q _))) n = isBlocked p n && isBlocked q n
isBlocked _ _ = False

step :: Maybe ExEvent -> Int -> RSP a -> (Maybe ExEvent, Int, RSP a, IO ())

-- push
step (Just event) n p
  = (Nothing, n + 1, bcast event p, pure ())
-- pop
step Nothing n p
  | n > 0 || isBlocked p n = (Nothing, n - 1, p, pure ())

-- emit
step Nothing n (RSP (Free (Emit e next)))
  = (Just e, n, RSP (Free (Canrun n next)), pure ())

-- canrun
step Nothing n (RSP (Free (Canrun m next)))
  | n == m = (Nothing, n, RSP next, pure ())

-- async
step Nothing n (RSP (Free (Async io next)))
  = (Nothing, n, RSP next, io)

-- and-expd
step Nothing n (RSP (Free (And p (RSP q) next)))
  = (Nothing, n, RSP (Free (AndU p (RSP (Free (Canrun n q))) next)), pure ())
-- and-adv1
step Nothing n (RSP (Free (AndU p q next)))
  | n == n' = (e, n, RSP (Free (AndU p' q next)), io)
  where
    (e, n', p', io) = step Nothing n p
-- and-adv2
step Nothing n (RSP (Free (AndU p q next)))
  |  isBlocked p n
  && n == n'= (e, n, RSP (Free (AndU p q' next)), io)
  where
    (e, n', q', io) = step Nothing n q
-- and-nop1
step Nothing n (RSP (Free (AndU (RSP (Pure a)) q next)))
  = (Nothing, n, q' a, pure ())
  where
   q' a = do
     b <- q
     RSP (next [a, b])
-- and-nop2
step Nothing n (RSP (Free (AndU p (RSP (Pure b)) next)))
  | isBlocked p n = (Nothing, n, p' b, pure ())
  where
    p' b = do
      a <- p
      RSP (next [a, b])

-- or-expd
step Nothing n (RSP (Free (Or p (RSP q) next)))
  = (Nothing, n, RSP (Free (OrU p (RSP (Free (Canrun n q))) next)), pure ())
-- or-adv1
step Nothing n (RSP (Free (OrU p q next)))
  | n == n' = (e, n, RSP (Free (OrU p' q next)), io)
  where
    (e, n', p', io) = step Nothing n p
-- or-adv2
step Nothing n (RSP (Free (OrU p q next)))
  | isBlocked p n
  && n == n'= (e, n, RSP (Free (OrU p q' next)), io)
  where
    (e, n', q', io) = step Nothing n q
-- or-nop1
step Nothing n (RSP (Free (OrU (RSP (Pure a)) q next)))
  = (Nothing, n, RSP (next a), pure ())
-- or-nop2
step Nothing n (RSP (Free (OrU p (RSP (Pure b)) next)))
  | isBlocked p n = (Nothing, n, RSP (next b), pure ())

step e n p = error (show p)

--------------------------------------------------------------------------------

run e n p = do
  traceIO (show p)
  let (e', n', p', io) = step e n p
  io
  case p' of
    RSP (Pure a) -> pure a
    _ -> run e' n' p'

runST = run Nothing 0

p1 = local $ \e -> runST $ do
  a <- andd [ Left <$> ((,) <$> await e <*> await e), Right <$> emit e "A", Right <$> emit e "C" ]
  async $ traceIO (show a)
  a <- orr [ Left <$> await e, Right <$> emit e "B" ]
  pure a

p2 = local $ \e -> runST $ do
  a <- andd [ Left <$> emit e "E", Right <$> await e ]
  pure a

p3 = local $ \e -> local $ \f -> runST $ do
  a <- andd
    [ Left  <$> (await e >> emit f "F")
    , Right <$> await f
    , Left  <$> emit e "E"
    ]
  pure a

p4 = local $ \e -> local $ \f -> runST $do
  a <- andd
    [ Left  <$> andd [ Left <$> await e, Right <$> emit f "F" ]
    , Right <$> await f
    , Left  <$> andd [ Left <$> pure "_", Right <$> (await f >> emit e "E") ]
    ]
  pure a

p5 = local $ \e -> runST $ do
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
      async $ traceIO "BLA"
      case a of
        Left n  -> go (s + n) e
        Right _ -> pure s

--------------------------------------------------------------------------------

-- many :: RSP (Either a [RSP ()]) -> RSP a
-- many k = do
--   go [ Left <$> k ]
--   where
--     go ks = do
--       a <- orr ks
--       undefined
-- 
-- done :: a -> RSP (Either a [RSP ()])
-- done a = pure (Left a)
-- 
-- spawn :: RSP () -> RSP (Either a [RSP ()])
-- spawn k = pure (Right [k])
-- 
-- data ST a
-- 
-- local' :: (ST a -> RSP b) -> RSP b
-- local' = undefined
-- 
-- with :: ST a -> (a -> [RSP a] -> Either a b) -> ST b
-- with = undefined

--------------------------------------------------------------------------------

-- testRSP :: RSP ()
-- testRSP = undefined
-- 
-- type Application a = (a -> IO Context) -> IO Context
-- 
-- server :: Application (Event String)
-- server app = do
--   e <- newEvent
--   ctx <- app e
--   emit ctx e "a"
--   emit ctx e "b"
--   pure ctx
-- 
-- m = server $ \x -> server $ \y -> server $ \z -> run $ do
--   a <- andd [ await x, await y, await z ]
--   async $ traceIO $ show a
--   a <- fst <$> orr [ await x, await y, await z ]
--   async $ traceIO a
-- 
-- m2 = server $ \x -> server $ \y -> server $ \z -> run $ do
--   a <- andd [ await x, await x, await x ]
--   async $ traceIO $ show a
--   a <- andd [ await x, await x, await x ]
--   async $ traceIO $ show a
-- 
-- boot :: Application (Event ())
-- boot app = do
--   boot <- newEvent
--   ctx <- app boot
--   emit ctx boot ()
--   pure ctx
-- 
-- m3 = boot $ \b -> run $ local $ \e -> do
--   await b
--   async $ traceIO "BOOTED"
--   a <- andd [ Left <$> awaitI e, Right <$> emitI e "asd" ]
--   async $ print a
