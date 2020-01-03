{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Syn where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Fail
import Control.Monad.Free

import Data.IORef
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe, mapMaybe)
import qualified Data.Map as M

import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafePerformIO)

import Debug.Trace

--------------------------------------------------------------------------------

data EventId = Internal Int | External Int
  deriving (Eq, Ord, Show)

data Internal
data External

data Event t a = Event EventId
  deriving Show

data EventValue = forall t a. EventValue (Event t a) a

instance Show EventValue where
  show (EventValue e _) = show e

data SynF next
  = Async (IO ()) next

  | Forever

  | forall a b. Local (Event Internal a -> Syn b) (b -> next)
  | Emit EventValue next
  | forall t a. Await (Event t a) (a -> next)

  | forall a. Or (Syn a) (Syn a) ((a, Syn a) -> next)
  | forall a b. And (Syn a) (Syn b) ((a, b) -> next)

deriving instance Functor SynF

newtype Syn a = Syn { getSyn :: Free SynF a }
  deriving (Functor, Applicative, Monad)

instance MonadFail Syn where
  fail e = error e

instance Alternative Syn where
  empty = forever
  a <|> b = orr [a, b]

instance Show (Syn a) where
  show (Syn (Pure a)) = "Pure"
  show (Syn (Free (Async _ _))) = "Async"
  show (Syn (Free Forever)) = "Forever"
  show (Syn (Free (Local _ _))) = "Local"
  show (Syn (Free (Emit (EventValue (Event e) _) _))) = "Emit (" <> show e <> ")"
  show (Syn (Free (Await (Event e) _))) = "Await (" <> show e <> ")"
  show (Syn (Free (Or a b _))) = "Or [" <> intercalate ", " (map show [a, b]) <> "]"
  show (Syn (Free (And a b _))) = "And [" <> show a <> ", " <> show b <> "]"

async :: IO () -> Syn ()
async io = Syn $ liftF (Async io ())

forever :: Syn a
forever = Syn $ liftF Forever

local :: (Event Internal a -> Syn b) -> Syn b
local f = Syn $ liftF (Local f id)

emit :: Event Internal a -> a -> Syn ()
emit e a = Syn $ liftF (Emit (EventValue e a) ())

await :: Event t a -> Syn a
await e = Syn $ liftF (Await e id)

-- | Left biased.
orr :: [Syn a] -> Syn a
orr [a] = a
orr [a, b] = fmap fst $ Syn $ liftF (Or a b id)
orr (a:as) = orr [a, orr as]

andd' :: [Syn a] -> Syn [a]
andd' [a] = (:[]) <$> a
andd' [a, b] = do
  (a, b) <- Syn $ liftF (And a b id)
  pure [a, b]
andd' (a:as) = concat <$> andd' [(:[]) <$> a, andd' as]

class Andd a b | a -> b where
  andd :: a -> b

instance Andd (Syn a, Syn b) (Syn (a, b)) where
  andd (a, b) = Syn $ liftF (And a b id)

instance Andd (Syn a, Syn b, Syn c) (Syn (a, b, c)) where
  andd (a, b, c) = do
    (k, (l, m)) <- andd (a, andd (b, c))
    pure (k, l, m)

instance Andd (Syn a, Syn b, Syn c, Syn d) (Syn (a, b, c, d)) where
  andd (a, b, c, d) = do
    (k, (l, m, n)) <- andd (a, andd (b, c, d))
    pure (k, l, m, n)

instance Andd (Syn a, Syn b, Syn c, Syn d, Syn e) (Syn (a, b, c, d, e)) where
  andd (a, b, c, d, e) = do
    (k, (l, m, n, o)) <- andd (a, andd (b, c, d, e))
    pure (k, l, m, n, o)

instance Andd (Syn a, Syn b, Syn c, Syn d, Syn e, Syn f) (Syn (a, b, c, d, e, f)) where
  andd (a, b, c, d, e, f) = do
    (k, (l, m, n, o, p)) <- andd (a, andd (b, c, d, e, f))
    pure (k, l, m, n, o, p)

--------------------------------------------------------------------------------

data Orr a = Orr (Syn (a, Orr a)) | D
  deriving Show

runOrr :: Orr a -> Maybe (Syn (a, Orr a))
runOrr (Orr o) = Just o
runOrr D = Nothing

instance Semigroup (Orr a) where
  D <> q = q
  p <> D = p
  Orr p <> Orr q = Orr $ do
    ((a, m), n) <- Syn (liftF (Or p q id))
    pure (a, m <> Orr n)

instance Monoid (Orr a) where
  mempty = D

liftOrr :: Syn a -> Orr a
liftOrr p = Orr ((,D) <$> p)

-- unblock ---------------------------------------------------------------------

unblock
  :: M.Map EventId EventValue
  -> Syn a
  -> (Syn a, Bool)

-- pure
unblock _ rsp@(Syn (Pure a)) = (rsp, False)

-- await
unblock m rsp@(Syn (Free (Await (Event eid') next)))
  = case M.lookup eid' m of
      Just (EventValue _ a) -> (Syn (next $ unsafeCoerce a), True)
      Nothing -> (rsp, False)

-- emit
unblock m rsp@(Syn (Free (Emit _ next))) = (Syn next, True)

-- and
unblock m rsp@(Syn (Free (And p q next)))
  = case (p', q') of
      (Syn (Pure a), Syn (Pure b))
        -> (Syn (next (a, b)), True)
      _ -> (Syn (Free (And p' q' next)), up || uq)
  where
    (p', up) = unblock m p
    (q', uq) = unblock m q

-- or
unblock m rsp@(Syn (Free (Or p q next)))
  = case (p', q') of
      (Syn (Pure a), _)
        -> (Syn (next (a, q')), True)
      (_, Syn (Pure b))
        -> (Syn (next (b, p')), True)
      _ -> (Syn (Free (Or p' q' next)), up || uq)
  where
    (p', up) = unblock m p
    (q', uq) = unblock m q

-- advance ---------------------------------------------------------------------

-- advance . advance == advance
advance
  :: Int
  -> [IO ()]
  -> Syn a
  -> (Int, [IO ()], Syn a)

-- pure
advance eid ios rsp@(Syn (Pure a))
  = (eid, ios, rsp)

-- await
advance eid ios rsp@(Syn (Free (Await _ _)))
  = (eid, ios, rsp)

-- local
advance eid ios (Syn (Free (Local f next)))
  = advance (eid + 1) ios (f (Event (Internal eid)) >>= Syn . next)

-- emit
advance eid ios rsp@(Syn (Free (Emit _ _)))
  = (eid, ios, rsp)

-- async
advance eid ios (Syn (Free (Async io next)))
  = advance (eid + 1) (io:ios) (Syn next)

-- and
advance eid ios rsp@(Syn (Free (And p q next)))
  = case (p', q') of
      (Syn (Pure a), Syn (Pure b))
        -> advance eid'' ios'' (Syn (next (a, b)))
      _ -> (eid'', ios'', Syn (Free (And p' q' next)))
  where
    (eid', ios', p') = advance (eid + 1) ios p
    (eid'', ios'', q') = advance (eid' + 1) ios' q

-- or
advance eid ios rsp@(Syn (Free (Or p q next)))
  = case (p', q') of
      (Syn (Pure a), _)
        -> advance eid' ios' (Syn (next (a, q')))
      (_, Syn (Pure b))
        -> advance eid'' ios'' (Syn (next (b, p')))
      _ -> (eid'', ios'', Syn (Free (Or p' q' next)))
  where
    (eid', ios', p') = advance (eid + 1) ios p
    (eid'', ios'', q') = advance (eid' + 1) ios' q

-- gather ----------------------------------------------------------------------

gather
  :: Syn a
  -> M.Map EventId EventValue

-- pure
gather (Syn (Pure _)) = M.empty

-- await
gather (Syn (Free (Await _ _))) = M.empty

-- emit
gather (Syn (Free (Emit e@(EventValue (Event ei) _) next))) = M.singleton ei e

-- and
gather (Syn (Free (And p q next))) = gather p <> gather q

-- or
gather (Syn (Free (Or p q next))) = gather p <> gather q

--------------------------------------------------------------------------------

stepOnce :: M.Map EventId EventValue -> Int -> Syn a -> IO (Int, Syn a, Bool)
stepOnce m' eid p = do
  sequence_ ios
  pure (eid', p'', u)
  where
    (eid', ios, p') = advance eid [] p
    m = gather p'
    (p'', u) = unblock (m' <> m) p'

stepAll :: M.Map EventId EventValue -> Int -> Syn a -> IO (Either a (Int, Syn a))
stepAll = go
  where
    go m eid p = do
      (eid', p', u) <- stepOnce m eid p

      -- traceIO ("*** " <> show p)
      -- traceIO ("### " <> show p' <> ", EVENTS: " <> show (M.keys m))

      case (p', u) of
        (Syn (Pure a), _) -> pure (Left a)
        (_, True) -> go M.empty eid' p'
        (_, False) -> pure (Right (eid', p'))

exhaust :: Syn a -> IO a
exhaust p = do
  r <- stepAll M.empty 0 p
  case r of
    Left a -> pure a
    Right _ -> error "Blocked"

-- Pools -----------------------------------------------------------------------

data Pool = Pool (Event Internal (Syn ()))

pool :: (Pool -> Syn a) -> Syn a
pool f = local $ \e -> go e $ mconcat
  [ liftOrr (Right . Left <$> await e)
  , liftOrr (Left <$> f (Pool e))
  ]
  where
    go e k = do
      (r, k') <- fromJust (runOrr k)

      case r of
        Left a -> pure a
        Right (Left p)  -> go e $ mconcat
          [ liftOrr (Right . Left <$> await e)
          , liftOrr (fmap (Right . Right) p)
          , k'
          ]
        Right (Right _) -> go e k'

spawn :: Pool -> Syn () -> Syn ()
spawn (Pool e) p = do
  emit e p

--------------------------------------------------------------------------------

{-# NOINLINE nextId #-}
nextId :: IORef Int
nextId = unsafePerformIO (newIORef 0)

newEvent :: IO (Event External b)
newEvent = Event . External <$> atomicModifyIORef' nextId (\eid -> (eid + 1, eid))

newtype Context a = Context (MVar (Maybe (Int, Syn a)))

type Application a r = (a -> IO (Context r)) -> IO (Context r)

run :: Syn a -> IO (Context a)
run p = Context <$> newMVar (Just (0, p))

push :: Context b -> Event External a -> a -> IO (Maybe b)
push (Context v) e@(Event ei) a = modifyMVar v $ \v -> case v of
  Just (eid, p) -> do
    r <- stepAll (M.singleton ei (EventValue e a)) eid p

    case r of
      Left a -> pure (Nothing, Just a)
      Right (eid', p') -> pure (Just (eid', p'), Nothing)

  _ -> pure (Nothing, Nothing)

event :: Application (Event External a) r
event app = newEvent >>= app
