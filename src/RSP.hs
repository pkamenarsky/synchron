{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module RSP where

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
  show (RSP (Free (Emit (ExEvent (Event e) _) _))) = "Emit (" <> show e <> ")"
  show (RSP (Free (Await (Event e) _))) = "Await (" <> show e <> ")"
  show (RSP (Free (Or a b _)))   = "Or [" <> intercalate ", " (map show [a, b]) <> "]"
  show (RSP (Free (And a b _)))  = "And [" <> intercalate ", " (map show [a, b]) <> "]"

async :: IO () -> RSP ()
async io = RSP $ liftF (Async io ())

forever :: RSP a
forever = RSP $ liftF Forever

local :: (Event a -> RSP b) -> RSP b
local f = RSP $ liftF (Local f id)

emit :: Event a -> a -> RSP ()
emit e a = RSP $ liftF (Emit (ExEvent e a) ())

await :: Event a -> RSP a
await e = RSP $ liftF (Await e id)

-- | Left biased.
orr :: [RSP a] -> RSP a
orr [a] = a
orr [a, b] = fmap fst $ RSP $ liftF (Or a b id)
orr (a:as) = orr [a, orr as]

andd :: [RSP a] -> RSP [a]
andd [a] = (:[]) <$> a
andd [a, b] = RSP $ liftF (And a b id)
andd (a:as) = concat <$> andd [(:[]) <$> a, andd as]

--------------------------------------------------------------------------------

{-# NOINLINE nextId #-}
nextId :: IORef Int
nextId = unsafePerformIO (newIORef 0)

newEvent :: IO (Event b)
newEvent = Event . External <$> atomicModifyIORef' nextId (\eid -> (eid + 1, eid))

newtype Context a = Context (MVar (Maybe (Int, RSP a)))

type Application a r = (a -> IO (Context r)) -> IO (Context r)

runRSP :: RSP a -> IO (Context a)
runRSP p = Context <$> newMVar (Just (0, p))

emitG :: Context b -> Event a -> a -> IO (Maybe b)
emitG (Context v) e@(Event ei) a = modifyMVar v $ \v -> case v of
  Just (eid, p) -> do
    r <- stepAll (M.singleton ei (ExEvent e a)) eid p

    case r of
      Left a -> pure (Nothing, Just a)
      Right (eid', p') -> pure (Just (eid', p'), Nothing)

  _ -> pure (Nothing, Nothing)

global :: Application (Event a) r
global app = newEvent >>= app

--------------------------------------------------------------------------------

data Orr a = Orr (RSP (a, Orr a)) | D
  deriving Show

runOrr :: Orr a -> Maybe (RSP (a, Orr a))
runOrr (Orr o) = Just o
runOrr D = Nothing

instance Semigroup (Orr a) where
  D <> q = q
  p <> D = p
  Orr p <> Orr q = Orr $ do
    ((a, m), n) <- RSP (liftF (Or p q id))
    pure (a, m <> Orr n)

instance Monoid (Orr a) where
  mempty = D

liftOrr :: RSP a -> Orr a
liftOrr p = Orr ((,D) <$> p)

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

-- advance . advance == advance
advance
  :: Int
  -> [IO ()]
  -> RSP a
  -> (Int, [IO ()], RSP a)

-- pure
advance eid ios rsp@(RSP (Pure a))
  = (eid, ios, rsp)

-- await
advance eid ios rsp@(RSP (Free (Await _ _)))
  = (eid, ios, rsp)

-- local
advance eid ios (RSP (Free (Local f next)))
  = advance (eid + 1) ios (f (Event (Internal eid)) >>= RSP . next)

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

step :: M.Map EventId ExEvent -> Int -> RSP a -> IO (Int, RSP a, Bool)
step m' eid p = do
  sequence_ ios
  pure (eid', p'', u)
  where
    (eid', ios, p') = advance eid [] p
    m = gather p'
    (p'', u) = unblock (m' <> m) p'

stepAll :: M.Map EventId ExEvent -> Int -> RSP a -> IO (Either a (Int, RSP a))
stepAll = go
  where
    go m eid p = do
      (eid', p', u) <- step m eid p

      -- traceIO ("*** " <> show p)
      -- traceIO ("### " <> show p' <> ", EVENTS: " <> show (M.keys m))

      case (p', u) of
        (RSP (Pure a), _) -> pure (Left a)
        (_, True) -> go M.empty eid' p'
        (_, False) -> pure (Right (eid', p'))

run :: RSP a -> IO a
run p = do
  r <- stepAll M.empty 0 p
  case r of
    Left a -> pure a
    Right _ -> error "Blocked"

-- Pools -----------------------------------------------------------------------

data Pool = Pool (Event (RSP ()))

pool :: Show a => (Pool -> RSP a) -> RSP a
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

spawn :: Pool -> RSP () -> RSP ()
spawn (Pool e) p = do
  emit e p
