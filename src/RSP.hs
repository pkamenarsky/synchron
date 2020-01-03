{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module RSP where

import Control.Concurrent
import Control.Monad.Fail
import Control.Monad.Free
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.State as ST
import qualified Control.Lens as L

import Data.Bifunctor (first, second)
import Data.IORef
import Data.List (intercalate)
import Data.Maybe (isJust, isNothing, listToMaybe, mapMaybe)
import qualified Data.Map as M

import Unsafe.Coerce (unsafeCoerce)

import Debug.Trace

--------------------------------------------------------------------------------

newtype EventIdInt = EventIdInt Int
  deriving (Num, Eq, Ord, Show)

newtype EventIdRef = EventIdRef (IORef ())
  deriving Eq

data EventId = I EventIdInt -- | R EventIdRef
  deriving (Eq, Ord, Show)

data Event a = Event EventId
  deriving Show

data ExEvent = forall a. ExEvent (Event a) a

instance Show ExEvent where
  show (ExEvent e _) = show e

data RSPF next
  = Async (IO ()) next

  | Forever

  | forall a b. Local (Event a -> RSP b) (b -> next)
  | Emit ExEvent next
  | forall a. Await (Event a) (a -> next)

  | forall a. Or (RSP a) (RSP a) ((a, RSP a) -> next)
  | forall a. And (RSP a) (RSP a) ([a] -> next)

deriving instance Functor RSPF

newtype RSP a = RSP { getRSP :: Free RSPF a }
  deriving (Functor, Applicative, Monad)

instance MonadFail RSP where
  fail e = error e

instance Show (RSP a) where
  show (RSP (Pure a))            = "Pure"
  show (RSP (Free (Async _ _)))  = "Async"
  show (RSP (Free Forever))      = "Forever"
  show (RSP (Free (Local _ _)))  = "Local"
  show (RSP (Free (Emit (ExEvent (Event (I (EventIdInt e))) _) _))) = "Emit (" <> show e <> ")"
  show (RSP (Free (Emit _ _)))   = "Emit"
  show (RSP (Free (Await (Event (I (EventIdInt e))) _))) = "Await (" <> show e <> ")"
  show (RSP (Free (Await _ _)))  = "Await"
  show (RSP (Free (Or a b _)))   = "Or [" <> intercalate ", " (map show [a, b]) <> "]"
  show (RSP (Free (And a b _)))  = "And [" <> intercalate ", " (map show [a, b]) <> "]"

async :: IO () -> RSP ()
async io = RSP $ liftF (Async io ())

forever :: RSP a
forever = RSP $ liftF Forever

-- global :: (Event a -> IO b) -> IO b
-- global f = do
--   e <- newIORef ()
--   f (Event (R $ EventIdRef e))

local :: (Event a -> RSP b) -> RSP b
local f = RSP $ liftF (Local f id)

emit :: Event a -> a -> RSP ()
emit e a = RSP $ liftF (Emit (ExEvent e a) ())

await :: Event a -> RSP a
await e = RSP $ liftF (Await e id)

data Orr a = Orr { runOrr :: RSP (a, Orr a) } | D
  deriving Show

instance Semigroup (Orr a) where
  D <> q = q
  p <> D = p
  Orr p <> Orr q = Orr $ do
    ((a, m), n) <- RSP (liftF (Or p q id))
    pure (a, m <> Orr n)

instance Monoid (Orr a) where
  mempty = D

singletonOrr :: RSP a -> Orr a
singletonOrr p = Orr $ do
  a <- p
  pure (a, D)

orr' :: Show a => [RSP a] -> RSP (a, [RSP a])
orr' [a] = trace ("ORR1: " <> show a) ((,[]) <$> a)
orr' [a, b] = do
  (a, k) <- RSP (liftF (Or a b id))
  pure $ trace ("ORR2: " <> show (a, [k])) (a, [k])
orr' [a, b] = second (:[]) <$> RSP (liftF (Or a b id))
orr' (a:as) = do
  (x, ks) <- orr' [ Left <$> a, Right <$> orr' as ]
  -- :: RSP (Either a (a, [RSP a]), [RSP (Either a (a, [RSP a]))])

  -- Left ends  :: (a, [(a, [RSP a])])
  -- Right ends :: ((a, [RSP a]), [RSP a])

  let i = ("ORR3: " <> show (orr' [ Left <$> a, Right <$> orr' as ]) <> ", X: " <> show (x, ks))
  case x of
    Left b -> trace (i <> ", R: " <> show (b, [r ks]))
      $ pure (b, [r ks])
    Right (b, bs) -> trace (i <> ", R: " <> show (b, fmap l ks <> bs))
      $ pure (b, fmap l ks <> bs)
  where
    r :: [RSP (Either a (a, [RSP a]))] -> RSP a
    r [p] = do
      r <- p
      case r of
        Right (a, ks) -> pure a

    l :: RSP (Either a (a, [RSP a])) -> RSP a
    l p = do
      r <- p
      case r of
        Left a -> pure a

data M a = M (RSP a, M a) | MD

orr'' :: RSP a -> RSP a -> RSP a -> RSP (a, [RSP a])
orr'' a b c = do
  (x, ks) <- orr_ (Left <$> a) (Right <$> orr_ b c)
  -- :: RSP (Either a (a, RSP a), RSP (Either a (a, RSP a)))

  -- Left ends  :: (a, RSP (a, RSP a)) -?-> RSP (a, [RSP a])
  -- Right ends :: ((a, RSP a), RSP a) -?-> RSP (a, [RSP a])

  case x of
    Left a -> undefined
    Right (a, ks') -> undefined
  where
    f :: RSP (Either a (a, RSP a)) -> RSP a
    f p = do
      r <- p
      case r of
        Left a -> error "f"
        Right (a, ks) -> undefined

orr_ :: RSP a -> RSP a -> RSP (a, RSP a)
orr_ = undefined

orr :: [RSP a] -> RSP a
orr [a] = a
orr [a, b] = fmap fst $ RSP $ liftF (Or a b id)
orr (a:as) = orr [a, orr as]

andd :: [RSP a] -> RSP [a]
andd [a] = (:[]) <$> a
andd [a, b] = RSP $ liftF (And a b id)
andd (a:as) = concat <$> andd [(:[]) <$> a, andd as]

testorr' = run $ local $ \e -> local $ \f -> local $ \g -> do
  [_, Right (b, ks)] <- andd [ Left <$> ((,,) <$> await e <*> await f <*> await g), Right <$> (advO (ks e f g)) ]
  async (traceIO $ "KS1: " <> show (b, ks))
  [_, Right (b, ks)] <- andd [ Left <$> ((,) <$> await f <*> await g), Right <$> (advO ks) ]
  async (traceIO $ "KS2: " <> show (b, ks))
  [a, Right (b, ks)] <- andd [ Left <$> (await g), Right <$> (advO ks) ]
  async (traceIO $ "KS3: " <> show (b, ks))
  pure a
  where
    ks e f g
      = singletonO (emit e () >> pure 1) `appendO`
        singletonO (emit f () >> emit f () >> pure 2) `appendO`
        singletonO (emit g () >> emit g () >> emit g () >> pure 3)

--------------------------------------------------------------------------------

unblock
  :: M.Map EventId ExEvent
  -> RSP a
  -> (RSP a, Bool)

-- pure
unblock _ rsp@(RSP (Pure a)) = (rsp, False)

-- await
unblock m rsp@(RSP (Free (Await (Event eid') next)))
  = case M.lookup eid' m of
      Just (ExEvent _ a) -> (RSP (next $ unsafeCoerce a), True)
      Nothing -> (rsp, False)

-- emit
unblock m rsp@(RSP (Free (Emit _ next))) = (RSP next, True)

-- and
unblock m rsp@(RSP (Free (And p q next)))
  = case (p', q') of
      (RSP (Pure a), RSP (Pure b))
        -> (RSP (next [a, b]), True)
      _ -> (RSP (Free (And p' q' next)), up || uq)
  where
    (p', up) = unblock m p
    (q', uq) = unblock m q

-- or
unblock m rsp@(RSP (Free (Or p q next)))
  = case (p', q') of
      (RSP (Pure a), _)
        -> (RSP (next (a, q')), True)
      (_, RSP (Pure b))
        -> (RSP (next (b, p')), True)
      _ -> (RSP (Free (Or p' q' next)), up || uq)
  where
    (p', up) = unblock m p
    (q', uq) = unblock m q

--------------------------------------------------------------------------------

advance
  :: EventIdInt
  -> [IO ()]
  -> RSP a
  -> (EventIdInt, [IO ()], RSP a)

-- pure
advance eid ios rsp@(RSP (Pure a))
  = (eid, ios, rsp)

-- await
advance eid ios rsp@(RSP (Free (Await _ _)))
  = (eid, ios, rsp)

-- local
advance eid ios (RSP (Free (Local f next)))
  = advance (eid + 1) ios (f (Event (I eid)) >>= RSP . next)

-- emit
advance eid ios rsp@(RSP (Free (Emit _ _)))
  = (eid, ios, rsp)

-- async
advance eid ios (RSP (Free (Async io next)))
  = advance (eid + 1) (io:ios) (RSP next)

-- and
advance eid ios rsp@(RSP (Free (And p q next)))
  = case (p', q') of
      (RSP (Pure a), RSP (Pure b))
        -> advance eid'' ios'' (RSP (next [a, b]))
      _ -> (eid'', ios'', RSP (Free (And p' q' next)))
  where
    (eid', ios', p') = advance (eid + 1) ios p
    (eid'', ios'', q') = advance (eid' + 1) ios' q

-- or
advance eid ios rsp@(RSP (Free (Or p q next)))
  = case (p', q') of
      (RSP (Pure a), _)
        -> advance eid' ios' (RSP (next (a, q')))
      (_, RSP (Pure b))
        -> advance eid'' ios'' (RSP (next (b, p')))
      _ -> (eid'', ios'', RSP (Free (Or p' q' next)))
  where
    (eid', ios', p') = advance (eid + 1) ios p
    (eid'', ios'', q') = advance (eid' + 1) ios' q

--------------------------------------------------------------------------------

gather
  :: RSP a
  -> M.Map EventId ExEvent

-- pure
gather (RSP (Pure _)) = M.empty

-- await
gather (RSP (Free (Await _ _))) = M.empty

-- emit
gather (RSP (Free (Emit e@(ExEvent (Event ei) _) next))) = M.singleton ei e

-- and
gather (RSP (Free (And p q next))) = gather p <> gather q

-- or
gather (RSP (Free (Or p q next))) = gather p <> gather q

--------------------------------------------------------------------------------

run :: RSP a -> IO a
run = go 0
  where
    go 100 p = error "END"
    go eid p = do
      -- traceIO ("*** " <> show p)
      let (eid', ios, p') = advance eid [] p
          m = gather p'
          (p'', u) = unblock m p'
      -- traceIO ("### " <> show p' <> ", EVENTS: " <> show (M.keys m))
      sequence_ ios
      case p'' of
        RSP (Pure a) -> pure a
        _ -> if u
          then go eid' p''
          else error "Blocked"

-- Pools -----------------------------------------------------------------------

data Pool = Pool (Event (RSP ()))

pool :: Show a => (Pool -> RSP a) -> RSP a
pool f = local $ \e -> go e
  (singletonO (Left <$> f (Pool e)) `appendO` singletonO (Right . Left <$> await e))
  where
    go e k = do
      (r, k') <- advO k

      case r of
        Left a -> pure a
        Right (Left p)  -> go e
          ( (singletonO (Right . Left <$> await e))
            `appendO`
            (singletonO (fmap (Right . Right) p))
            `appendO`
            k'
          )
        Right (Right _) -> go e k'

spawn :: Pool -> RSP () -> RSP ()
spawn (Pool e) p = do
  emit e p
