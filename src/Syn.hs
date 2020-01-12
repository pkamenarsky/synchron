{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Syn where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Fail
import Control.Monad.Free

import Data.Proxy
import Data.Type.Equality

import Type.Reflection

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

data Trail v a = Trail
  { notify :: ([EventId], M.Map EventId EventValue) -> IO ()
  , p      :: MVar (Syn v a)
  }

data SynF v next
  = Async (IO ()) next

  | Forever

  | View next

  | forall u a. MapView (u -> v) (Syn u a)

  | forall a. Remote (IO (Trail v a)) (a -> next)
  | forall a. RemoteU (Trail v a) (a -> next)

  | forall a b. Local (Event Internal a -> next)
  | Emit EventValue next
  | forall t a. Await (Event t a) (a -> next)

  | forall a. Or (Syn v a) (Syn v a) ((a, Syn v a) -> next)
  | forall a b. And (Syn v a) (Syn v b) ((a, b) -> next)

  | forall a b. And_T (Trail v a) (Trail v b) ((a, b) -> next)

deriving instance Functor (SynF v)

mapViewF :: (u -> v) -> Syn u a -> SynF v a
mapViewF f m = MapView f m

data Syn v a = forall u. Monoid u => Syn
  { getView :: u
  , getMapping :: (u -> v)
  , getSyn :: Free (SynF u) a
  }

instance Functor (Syn v) where
  fmap f (Syn v fv m) = Syn v fv (fmap f m)

instance Monoid v => Applicative (Syn v) where
  pure a = Syn mempty id (Pure a)
  Syn _ _ f <*> Syn v vf a = Syn (vf v) id (_ f <*> _ a)

instance Monoid v => Monad (Syn v) where
  Syn _ _ m >>= f = undefined

instance Monoid v => MonadFail (Syn v) where
  fail e = error e

instance (Typeable v, Monoid v) => Alternative (Syn v) where
  empty = forever
  a <|> b = orr [a, b]

instance Show (Syn v a) where
  show (Syn _ _ (Pure a)) = "Pure"
  show (Syn _ _ (Free (Async _ _))) = "Async"
  show (Syn _ _ (Free Forever)) = "Forever"
  show (Syn _ _ (Free (View _))) = "View"
  show (Syn _ _ (Free (Local _))) = "Local"
  show (Syn _ _ (Free (Emit (EventValue (Event e) _) _))) = "Emit (" <> show e <> ")"
  show (Syn _ _ (Free (Await (Event e) _))) = "Await (" <> show e <> ")"
  show (Syn _ _ (Free (Or a b _))) = "Or [" <> intercalate ", " (map show [a, b]) <> "]"
  show (Syn _ _ (Free (And a b _))) = "And [" <> show a <> ", " <> show b <> "]"

mapView :: (u -> v) -> Syn u a -> Syn v a
mapView g (Syn v f m) = Syn v (g . f) m
--mapView f (Syn _ _ m) = Syn (hoistFree (go f) m)
--  where
--    go f (Async io next) = Async io next
--    go f Forever = Forever
--    go f (Emit e next) = Emit e next
--    go f (View v next) = View (f v) next
--    go f (Local k next) = Local (\e -> mapView f (k e)) next
--    go f (Await e next) = Await e next
--    go f (Or u p q next) = Or (f . u) p q next
--    go f (And p q next) = And (mapView f p) (mapView f q) next

async :: Monoid v => IO () -> Syn v ()
async io = Syn mempty id $ liftF (Async io ())

forever :: Monoid v => Syn v a
forever = Syn mempty id $ liftF Forever

view :: Monoid v => v -> Syn v ()
view v = Syn v id $ liftF (View ())

local :: Monoid v => (Event Internal a -> Syn v b) -> Syn v b
local f = do
  e <- Syn mempty id $ liftF (Local id)
  f e

emit :: Monoid v => Event Internal a -> a -> Syn v ()
emit e a = Syn mempty id $ liftF (Emit (EventValue e a) ())

await :: Monoid v => Event t a -> Syn v a
await e = Syn mempty id $ liftF (Await e id)

-- | Left biased.
orr :: Typeable v => Monoid v => [Syn v a] -> Syn v a
orr [a] = a
orr [a, b] = fmap fst $ Syn mempty id $ liftF (Or a b id)
orr (a:as) = orr [a, orr as]

andd' :: Monoid v => [Syn v a] -> Syn v [a]
andd' [a] = (:[]) <$> a
andd' [a, b] = do
  (a, b) <- Syn mempty id $ liftF (And a b id)
  pure [a, b]
andd' (a:as) = concat <$> andd' [(:[]) <$> a, andd' as]

class Andd a b | a -> b, b -> a where
  andd :: a -> b

instance Monoid v => Andd (Syn v a, Syn v b) (Syn v (a, b)) where
  andd (a, b) = Syn mempty id $ liftF (And a b id)

instance Monoid v => Andd (Syn v a, Syn v b, Syn v c) (Syn v (a, b, c)) where
  andd (a, b, c) = do
    (k, (l, m)) <- andd (a, andd (b, c))
    pure (k, l, m)

instance Monoid v => Andd (Syn v a, Syn v b, Syn v c, Syn v d) (Syn v (a, b, c, d)) where
  andd (a, b, c, d) = do
    (k, (l, m, n)) <- andd (a, andd (b, c, d))
    pure (k, l, m, n)

instance Monoid v => Andd (Syn v a, Syn v b, Syn v c, Syn v d, Syn v e) (Syn v (a, b, c, d, e)) where
  andd (a, b, c, d, e) = do
    (k, (l, m, n, o)) <- andd (a, andd (b, c, d, e))
    pure (k, l, m, n, o)

instance Monoid v => Andd (Syn v a, Syn v b, Syn v c, Syn v d, Syn v e, Syn v f) (Syn v (a, b, c, d, e, f)) where
  andd (a, b, c, d, e, f) = do
    (k, (l, m, n, o, p)) <- andd (a, andd (b, c, d, e, f))
    pure (k, l, m, n, o, p)

--------------------------------------------------------------------------------

data Orr v a = Orr (Syn v (a, Orr v a)) | D
  deriving (Functor, Show)

runOrr :: Orr v a -> Maybe (Syn v (a, Orr v a))
runOrr (Orr o) = Just o
runOrr D = Nothing

unsafeRunOrr :: Orr v a -> Syn v (a, Orr v a)
unsafeRunOrr (Orr o) = o
unsafeRunOrr D = error "unsafeRunOrr: D"

instance (Typeable v, Monoid v) => Semigroup (Orr v a) where
  D <> q = q
  p <> D = p
  Orr p <> Orr q = Orr $ do
    ((a, m), n) <- Syn mempty id (liftF (Or p q id))
    pure (a, m <> Orr n)

instance (Typeable v, Monoid v) => Monoid (Orr v a) where
  mempty = D

liftOrr :: Syn v a -> Orr v a
liftOrr p = Orr ((,D) <$> p)

-- unblock ---------------------------------------------------------------------

unblock
  :: Semigroup v
  => M.Map EventId EventValue
  -> Syn v a
  -> (Syn v a, Bool)

-- pure
unblock _ rsp@(Syn _ _ (Pure a)) = (rsp, False)

-- forever
unblock _ rsp@(Syn _ _ (Free Forever)) = (rsp, False)

-- await
unblock m rsp@(Syn v f (Free (Await (Event eid') next)))
  = case M.lookup eid' m of
      Just (EventValue _ a) -> (Syn v f (next $ unsafeCoerce a), True)
      Nothing -> (rsp, False)

-- emit
unblock m rsp@(Syn v f (Free (Emit _ next))) = (Syn v f next, True)

-- and
unblock m rsp@(Syn v f (Free (And p q next)))
  = case (p', q') of
      (Syn pv pf (Pure a), Syn qv qf (Pure b))
        -> (Syn (pf pv <> qf qv) f (next (a, b)), True)
      _ -> (Syn v f (Free (And p' q' next)), up || uq)
  where
    (p', up) = unblock m p
    (q', uq) = unblock m q

-- or
unblock m rsp@(Syn v f (Free (Or p q next)))
  = case (p', q') of
      (Syn pv pf (Pure a), _)
        -> (Syn (pf pv) f (next (a, q')), True)
      (_, Syn qv qf (Pure b))
        -> (Syn (qf qv) f (next (b, p')), True)
      _ -> (Syn v f (Free (Or p' q' next)), up || uq)
  where
    (p', up) = unblock m p
    (q', uq) = unblock m q

-- advance ---------------------------------------------------------------------

data V v = E | V v | forall u. (Monoid u, Typeable u) => P (u -> v) (V u) (V u) | forall u. (Monoid u, Typeable u) => U (u -> v) (V u)

deriving instance Functor V

foldV :: Monoid v => V v -> v
foldV E = mempty
foldV (V v) = v
foldV (U u v) = foldV (u <$> v)
foldV (P u p q) = foldV (u <$> p) <> foldV (u <$> q)

-- advance . advance == advance
advance
  :: Monoid v
  => Int
  -> [IO ()]
  -> Syn v a
  -> (Int, [IO ()], Syn v a)

-- pure
advance eid ios rsp@(Syn _ _ (Pure a))
  = (eid, ios, rsp)

-- forever
advance eid ios rsp@(Syn _ _ (Free Forever))
  = (eid, ios, rsp)

-- await
advance eid ios rsp@(Syn _ _ (Free (Await _ _)))
  = (eid, ios, rsp)

-- view
advance eid ios rsp@(Syn v f (Free (View next)))
  = advance (eid + 1) ios (Syn v f next)

-- local
advance eid ios (Syn v f (Free (Local fe)))
  = advance (eid + 1) ios (Syn v f $ fe (Event (Internal eid)))

-- emit
advance eid ios rsp@(Syn _ _ (Free (Emit _ _)))
  = (eid, ios, rsp)

-- async
advance eid ios (Syn v f (Free (Async io next)))
  = advance (eid + 1) (io:ios) (Syn v f next)

-- and
advance eid ios rsp@(Syn v f (Free (And p q next)))
  = case (p', q') of
      (Syn pv pf (Pure a), Syn qv qf (Pure b))
           -- TODO: should we foldV here? or use parallel Is?
        -> advance eid'' ios'' (Syn (pf pv <> qf qv) f (next (a, b)))
      _ -> (eid'', ios'', Syn v f (Free (And p' q' next)))
  where
    (eid', ios', p') = advance (eid + 1) ios p
    (eid'', ios'', q') = advance (eid' + 1) ios' q

-- or
-- advance eid ios rsp@(Syn (Free (Or u p q next))) v
--   = case (p', q') of
--       (Syn (Pure a), _)
--         -> advance eid' ios' (Syn (next (a, q'))) (u <$> V id (foldV pv'))
--       (_, Syn (Pure b))
--         -> advance eid'' ios'' (Syn (next (b, p'))) (u <$> V id (foldV qv'))
--       _ -> (eid'', ios'', Syn (Free (Or u p' q' next)), unsafeCoerce v')
--   where
--     v' = case (pv', qv') of
--       (E, E) -> undefined -- v
--       (_, _) -> P pv' qv'
-- 
--     (pv, qv) = case v of
--       P pv qv -> (pv, qv)
--       _ -> (E, E)
-- 
--     (eid', ios', p', pv') = advance (eid + 1) ios p (_ pv)
--     (eid'', ios'', q', qv') = advance (eid' + 1) ios' q qv

-- advance :: Int -> [IO ()] -> Syn v a -> V v -> (Int, [IO ()], Syn v a, V v)
-- advance eid ios (Syn (Free (Or (u :: u -> v) p q next))) v@(P (u' :: u' -> v') pv qv) = case (testEquality (typeRep :: TypeRep (u -> v)) (typeRep :: TypeRep (u' -> v'))) of
--   Just Refl ->
--     let (eid', ios', p', pv') = advance (eid + 1) ios p pv
--         (eid'', ios'', q', qv') = advance (eid' + 1) ios' q qv
-- 
--         v' = case (pv', qv') of
--           (E, E) -> v
--           (_, _) -> P u pv' qv'
--         
--     in case (p', q') of
--       (Syn (Pure a), _)
--         -> advance eid' ios' (Syn (next (a, q'))) (U u pv')
--       (_, Syn (Pure b))
--         -> advance eid'' ios'' (Syn (next (b, p'))) (U u qv')
--       _ -> (eid'', ios'', Syn (Free (Or u p' q' next)), v')
--   Nothing -> error "NOT REFL"

-- advance eid ios rsp@(Syn (Free (Or u p q next))) v@(P u' pv qv)
--   = case (p', q') of
--       (Syn (Pure a), _)
--         -> advance eid' ios' (Syn (next (a, q'))) (u <$> V (foldV pv'))
--       (_, Syn (Pure b))
--         -> advance eid'' ios'' (Syn (next (b, p'))) (u <$> V (foldV qv'))
--       _ -> (eid'', ios'', Syn (Free (Or u p' q' next)), v')
--   where
--     v' = case (pv', qv') of
--       (E, E) -> v
--       (_, _) -> P u' (unsafeCoerce pv') (unsafeCoerce qv')
-- 
--     (eid', ios', p', pv') = advance (eid + 1) ios p (unsafeCoerce pv)
--     (eid'', ios'', q', qv') = advance (eid' + 1) ios' q (unsafeCoerce qv)

advance eid ios rsp@(Syn v f (Free (Or p q next)))
  = case (p', q') of
      (Syn pv pf (Pure a), _)
        -> advance eid' ios' (Syn (pf pv) f (next (a, q')))
      (_, Syn qv qf (Pure b))
        -> advance eid'' ios'' (Syn (qf qv) f (next (b, p')))
      _ -> (eid'', ios'', Syn v f (Free (Or p' q' next)))
  where
    (eid', ios', p') = advance (eid + 1) ios p
    (eid'', ios'', q') = advance (eid' + 1) ios' q

-- gather ----------------------------------------------------------------------

gather
  :: Syn v a
  -> M.Map EventId EventValue

-- pure
gather (Syn _ _ (Pure _)) = M.empty

-- forever
gather (Syn _ _ (Free Forever)) = M.empty

-- await
gather (Syn _ _ (Free (Await _ _))) = M.empty

-- emit
gather (Syn _ _ (Free (Emit e@(EventValue (Event ei) _) next))) = M.singleton ei e

-- and
gather (Syn _ _ (Free (And p q next))) = gather q <> gather p

-- or
gather (Syn _ _ (Free (Or p q next))) = gather q <> gather p

--------------------------------------------------------------------------------

stepOnce :: Typeable v => Monoid v => M.Map EventId EventValue -> Int -> Syn v a -> IO (Int, Syn v a, v, [EventId], Bool)
stepOnce m' eid p = do
  sequence_ ios
  case p'' of
    Syn v f _ -> pure (eid', p'', f v, M.keys m, u)
  where
    (eid', ios, p') = advance eid [] p
    m = gather p'
    (p'', u) = unblock (m' <> m) p'

stepAll :: Typeable v => Monoid v => M.Map EventId EventValue -> Int -> Syn v a -> IO (Either (Maybe a) (Int, Syn v a), v, [([EventId], Syn v a)])
stepAll = go []
  where
    go es m eid p = do
      (eid', p', v', eks, u) <- stepOnce m eid p

      -- traceIO ("*** " <> show p)
      -- traceIO ("### " <> show p' <> ", EVENTS: " <> show (M.keys m) <> ", U: " <> show u)

      case (p', u) of
        (Syn _ _ (Pure a), _) -> pure (Left (Just a), v', (eks, p):es)
        (Syn _ _ (Free Forever), _) -> pure (Right (eid', p'), v', (eks, p):es) -- pure (Left (Nothing, v'))
        (_, True) -> go ((eks ,p):es) M.empty eid' p'
        (_, False) -> pure (Right (eid', p'), v', (eks, p):es)

exhaust :: Typeable v => Monoid v => Syn v a -> IO (Maybe a, v)
exhaust p = do
  r <- stepAll M.empty 0 p
  case r of
    (Left a, v, _)  -> pure (a, v)
    (Right _, _, _) -> error "Blocked"

-- Pools -----------------------------------------------------------------------

data Pool v = Pool (Event Internal (Syn v ()))

pool :: Typeable v => Monoid v => (Pool v -> Syn v a) -> Syn v a
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

spawn :: Monoid v => Pool v -> Syn v () -> Syn v ()
spawn (Pool e) p = do
  emit e p

--------------------------------------------------------------------------------

{-# NOINLINE nextId #-}
nextId :: IORef Int
nextId = unsafePerformIO (newIORef 0)

newEvent :: IO (Event External b)
newEvent = Event . External <$> atomicModifyIORef' nextId (\eid -> (eid + 1, eid))

newtype Context v a = Context (MVar (Maybe (Int, Syn v a)))

type Application v a r = (a -> IO (Context v r)) -> IO (Context v r)

run :: Syn v a -> IO (Context v a)
run p = Context <$> newMVar (Just (0, p))

push :: Typeable v => Monoid v => Context v b -> Event t a -> a -> IO (Maybe b, v)
push (Context v) e@(Event ei) a = modifyMVar v $ \v -> case v of
  Just (eid, p) -> do
    r <- stepAll (M.singleton ei (EventValue e a)) eid p

    case r of
      (Left a, v, _) -> pure (Nothing, (a, v))
      (Right (eid', p'), v', _) -> pure (Just (eid', p'), (Nothing, v'))

  _ -> pure (Nothing, (Nothing, mempty))

event :: Application v (Event External a) r
event app = newEvent >>= app
