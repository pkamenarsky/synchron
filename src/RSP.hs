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

import Data.Bifunctor (second)
import Data.IORef
import Data.List (intercalate)
import Data.Maybe (listToMaybe, mapMaybe)

import Unsafe.Coerce (unsafeCoerce)

import Debug.Trace

--------------------------------------------------------------------------------

newtype EventIdInt = EventIdInt Int
  deriving (Num, Eq, Ord, Show)

newtype EventIdRef = EventIdRef (IORef ())
  deriving Eq

data EventId = I EventIdInt | R EventIdRef
  deriving Eq

data Event a = Event EventId

data ExEvent = forall a. ExEvent (Event a) a

newtype StackLevel = StackLevel Int
  deriving (Eq, Ord, Num, Show)

data RSPF next
  = Async (IO ()) next

  | Forever

  | forall a b. Local (Event a -> RSP b) (b -> next)
  | Emit ExEvent next
  | forall a. Await (Event a) (a -> next)

  | forall a. Or (RSP a) (RSP a) ((a, RSP a) -> next)
  | forall a. And (RSP a) (RSP a) ([a] -> next)

  | forall a. OrU (RSP a) (RSP a) ((a, RSP a) -> next)
  | forall a. AndU (RSP a) (RSP a) ([a] -> next)

  | forall a. Canrun StackLevel next

deriving instance Functor RSPF

newtype RSP a = RSP { getRSP :: Free RSPF a }
  deriving (Functor, Applicative, Monad)

instance Show (RSP a) where
  show (RSP (Pure a))            = "Pure"
  show (RSP (Free (Async _ _)))  = "Async"
  show (RSP (Free Forever))      = "Forever"
  show (RSP (Free (Local _ _)))  = "Local"
  show (RSP (Free (Emit _ _)))   = "Emit"
  show (RSP (Free (Await _ _)))  = "Await"
  show (RSP (Free (Or a b _)))   = "Or [" <> intercalate ", " (map show [a, b]) <> "]"
  show (RSP (Free (And a b _)))  = "And [" <> intercalate ", " (map show [a, b]) <> "]"
  show (RSP (Free (OrU a b _)))  = "OrU [" <> intercalate ", " (map show [a, b]) <> "]"
  show (RSP (Free (AndU a b _))) = "AndU [" <> intercalate ", " (map show [a, b]) <> "]"
  show (RSP (Free (Canrun (StackLevel n) p))) = "Canrun " <> show n <> " (" <> show (RSP p) <> ")"

async :: IO () -> RSP ()
async io = RSP $ liftF (Async io ())

forever :: RSP a
forever = RSP $ liftF Forever

global :: (Event a -> IO b) -> IO b
global f = do
  e <- newIORef ()
  f (Event (R $ EventIdRef e))

local :: (Event a -> RSP b) -> RSP b
local f = RSP $ liftF (Local f id)

emit :: Event a -> a -> RSP ()
emit e a = RSP $ liftF (Emit (ExEvent e a) ())

await :: Event a -> RSP a
await e = RSP $ liftF (Await e id)

orr' :: [RSP a] -> RSP (a, [RSP a])
orr' [a] = (,[]) <$> a
orr' [a, b] = second (:[]) <$> RSP (liftF (Or a b id))
orr' (a:as) = do
  r <- orr' [Left <$> a, Right <$> orr' as]
  case r of
    (Left x, _)        -> pure (x, as)
    (Right (x, ks), _) -> pure (x, [a] <> ks)

orr :: [RSP a] -> RSP a
orr [a] = a
orr [a, b] = fmap fst $ RSP $ liftF (Or a b id)
orr (a:as) = orr [a, orr as]

andd :: [RSP a] -> RSP [a]
andd [a] = (:[]) <$> a
andd [a, b] = RSP $ liftF (And a b id)
andd (a:as) = concat <$> andd [(:[]) <$> a, andd as]

--------------------------------------------------------------------------------

bcast :: ExEvent -> RSP a -> RSP a
bcast (ExEvent (Event e) a) (RSP (Free (Await (Event e') next)))
  | e == e' = RSP (next $ unsafeCoerce a)
bcast e (RSP (Free (AndU p q next)))
  = RSP (Free (AndU (bcast e p) (bcast e q) next))
-- TODO
bcast e (RSP (Free (And p q next)))
  = RSP (Free (And (bcast e p) (bcast e q) next))
bcast e (RSP (Free (OrU p q next)))
  = RSP (Free (OrU (bcast e p) (bcast e q) next))
-- TODO
bcast e (RSP (Free (Or p q next)))
  = RSP (Free (Or (bcast e p) (bcast e q) next))
-- TODO
bcast e (RSP (Free (Canrun n next)))
  = RSP (Free (Canrun n (getRSP $ bcast e (RSP next))))
bcast _ p = p

isBlocked :: RSP a -> StackLevel -> Bool
isBlocked (RSP (Free (Await _ _))) _ = True
isBlocked (RSP (Free (Canrun m _))) n = n > m
isBlocked (RSP (Free (AndU p q _))) n = isBlocked p n && isBlocked q n
isBlocked (RSP (Free (OrU p q _))) n = isBlocked p n && isBlocked q n
isBlocked _ _ = False

step
  :: Maybe ExEvent
  -> StackLevel
  -> EventIdInt
  -> RSP a
  -> (Maybe ExEvent, StackLevel, EventIdInt, RSP a, IO ())

-- push
step (Just event) n eid p
  = (Nothing, n + 1, eid + 1, bcast event p, pure ())
-- pop
step Nothing n eid p
  | n > 0 && isBlocked p n = (Nothing, n - 1, eid + 1, p, pure ())

-- local
step Nothing n eid (RSP (Free (Local f next)))
  = (Nothing, n, eid + 1, f (Event (I eid)) >>= RSP . next, pure ())

-- emit
step Nothing n eid (RSP (Free (Emit e next)))
  = (Just e, n, eid + 1, RSP (Free (Canrun n next)), pure ())

-- canrun
step Nothing n eid (RSP (Free (Canrun m next)))
  | n == m = (Nothing, n, eid + 1, RSP next, pure ())

-- async
step Nothing n eid (RSP (Free (Async io next)))
  = (Nothing, n, eid + 1, RSP next, io)

-- and-expd
step Nothing n eid (RSP (Free (And p (RSP q) next)))
  = (Nothing, n, eid + 1, RSP (Free (AndU p (RSP (Free (Canrun n q))) next)), pure ())
-- and-nop1
step Nothing n eid (RSP (Free (AndU (RSP (Pure a)) q next)))
  = (Nothing, n, eid + 1, q' a, pure ())
  where
   q' a = do
     b <- q
     RSP (next [a, b])
-- and-nop2
step Nothing n eid (RSP (Free (AndU p (RSP (Pure b)) next)))
  | isBlocked p n = (Nothing, n, eid + 1, p' b, pure ())
  where
    p' b = do
      a <- p
      RSP (next [a, b])
-- and-adv2
step Nothing n eid (RSP (Free (AndU p q next)))
  | isBlocked p n
  && n == n' = (e, n, eid', RSP (Free (AndU p q' next)), io)
  where
    (e, n', eid', q', io) = step Nothing n eid q
-- and-adv1
step Nothing n eid (RSP (Free (AndU p q next)))
  | n == n' = (e, n, eid', RSP (Free (AndU p' q next)), io)
  where
    (e, n', eid', p', io) = step Nothing n eid p

-- or-expd
step Nothing n eid (RSP (Free (Or p (RSP q) next)))
  = (Nothing, n, eid + 1, RSP (Free (OrU p (RSP (Free (Canrun n q))) next)), pure ())
-- or-nop1
step Nothing n eid (RSP (Free (OrU (RSP (Pure a)) q next)))
  = (Nothing, n, eid + 1, RSP (next (a, q)), pure ())
-- or-nop2
step Nothing n eid (RSP (Free (OrU p (RSP (Pure b)) next)))
  | isBlocked p n = (Nothing, n, eid + 1, RSP (next (b, p)), pure ())
-- or-adv2
step Nothing n eid (RSP (Free (OrU p q next)))
  | isBlocked p n
  && n == n' = (e, n, eid', RSP (Free (OrU p q' next)), io)
  where
    (e, n', eid', q', io) = step Nothing n eid q
-- or-adv1
step Nothing n eid (RSP (Free (OrU p q next)))
  | n == n' = (e, n, eid', RSP (Free (OrU p' q next)), io)
  where
    (e, n', eid', p', io) = step Nothing n eid p

step e eid n p = error (isEvent e <> ", " <> show n <> ", " <> show eid <> ", " <> show p)
  where
    isEvent (Just _) = "Event"
    isEvent Nothing  = "No event"

--------------------------------------------------------------------------------

run :: RSP a -> IO a
run = go Nothing 0 0
  where
    go e n eid p = do
      let (e', n', eid', p', io) = step e n eid p
      io
      case p' of
        RSP (Pure a) -> pure a
        _ -> go e' n' eid' p'

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
  
