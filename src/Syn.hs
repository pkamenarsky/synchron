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
  { trNotify  :: M.Map EventId EventValue -> IO ()
  , trAdvance :: IO (Maybe a, V v)
  , trGather  :: IO (M.Map EventId EventValue)
  , trUnblock :: M.Map EventId EventValue -> IO Bool
  }

data SynF v next
  = Async (IO ()) next

  | Forever

  | View v next

  | forall u a. Monoid u => MapView (u -> v) (Syn u a) (a -> next)

  | forall a. Remote (IO (Trail v a)) (a -> next)
  | forall a. RemoteU (Trail v a) (a -> next)

  | forall a b. Local (Event Internal a -> Syn v b) (b -> next)
  | Emit EventValue next
  | forall t a. Await (Event t a) (a -> next)

  | forall a. Or (Syn v a) (Syn v a) ((a, (Syn v a, V v)) -> next)
  | forall a b. And (Syn v a) (Syn v b) ((a, b) -> next)

  | forall a b. And_T (Trail v a) (Trail v b) ((a, b) -> next)

deriving instance Functor (SynF v)

newtype Syn v a = Syn { getSyn :: Free (SynF v) a }
  deriving (Functor, Applicative, Monad)

instance MonadFail (Syn v) where
  fail e = error e

instance (Monoid v) => Alternative (Syn v) where
  empty = forever
  a <|> b = orr [a, b]

instance Show (Syn v a) where
  show (Syn (Pure a)) = "Pure"
  show (Syn (Free (Async _ _))) = "Async"
  show (Syn (Free Forever)) = "Forever"
  show (Syn (Free (MapView _ m _))) = "MapView (" <> show m <> ")"
  show (Syn (Free (View _ _))) = "View"
  show (Syn (Free (Local _ _))) = "Local"
  show (Syn (Free (Emit (EventValue (Event e) _) _))) = "Emit (" <> show e <> ")"
  show (Syn (Free (Await (Event e) _))) = "Await (" <> show e <> ")"
  show (Syn (Free (Or a b _))) = "Or [" <> intercalate ", " (map show [a, b]) <> "]"
  show (Syn (Free (And a b _))) = "And [" <> show a <> ", " <> show b <> "]"

mapView :: Monoid u => (u -> v) -> Syn u a -> Syn v a
mapView f m = Syn $ liftF (MapView f m id)

async :: IO () -> Syn v ()
async io = Syn $ liftF (Async io ())

forever :: Syn v a
forever = Syn $ liftF Forever

view :: v -> Syn v ()
view v = Syn $ liftF (View v ())

local :: (Event Internal a -> Syn v b) -> Syn v b
local f = Syn $ liftF (Local f id)

emit :: Event Internal a -> a -> Syn v ()
emit e a = Syn $ liftF (Emit (EventValue e a) ())

await :: Event t a -> Syn v a
await e = Syn $ liftF (Await e id)

-- | Left biased.
orr :: Monoid v => [Syn v a] -> Syn v a
orr [a] = a
orr [a, b] = fmap fst $ Syn $ liftF (Or a b id)
orr (a:as) = orr [a, orr as]

orr' :: Monoid v => Syn v a -> Syn v a -> Syn v (a, (Syn v a, V v))
orr' a b = Syn $ liftF (Or a b id)

andd' :: [Syn v a] -> Syn v [a]
andd' [a] = (:[]) <$> a
andd' [a, b] = do
  (a, b) <- Syn $ liftF (And a b id)
  pure [a, b]
andd' (a:as) = concat <$> andd' [(:[]) <$> a, andd' as]

class Andd a b | a -> b, b -> a where
  andd :: a -> b

instance Andd (Syn v a, Syn v b) (Syn v (a, b)) where
  andd (a, b) = Syn $ liftF (And a b id)

instance Andd (Syn v a, Syn v b, Syn v c) (Syn v (a, b, c)) where
  andd (a, b, c) = do
    (k, (l, m)) <- andd (a, andd (b, c))
    pure (k, l, m)

instance Andd (Syn v a, Syn v b, Syn v c, Syn v d) (Syn v (a, b, c, d)) where
  andd (a, b, c, d) = do
    (k, (l, m, n)) <- andd (a, andd (b, c, d))
    pure (k, l, m, n)

instance Andd (Syn v a, Syn v b, Syn v c, Syn v d, Syn v e) (Syn v (a, b, c, d, e)) where
  andd (a, b, c, d, e) = do
    (k, (l, m, n, o)) <- andd (a, andd (b, c, d, e))
    pure (k, l, m, n, o)

instance Andd (Syn v a, Syn v b, Syn v c, Syn v d, Syn v e, Syn v f) (Syn v (a, b, c, d, e, f)) where
  andd (a, b, c, d, e, f) = do
    (k, (l, m, n, o, p)) <- andd (a, andd (b, c, d, e, f))
    pure (k, l, m, n, o, p)

--------------------------------------------------------------------------------

data Orr v a = Orr (Syn v (a, V v, Orr v a)) | D
  deriving (Functor, Show)

runOrr :: Orr v a -> Maybe (Syn v (a, V v, Orr v a))
runOrr (Orr o) = Just o
runOrr D = Nothing

unsafeRunOrr :: Orr v a -> Syn v (a, V v, Orr v a)
unsafeRunOrr (Orr o) = o
unsafeRunOrr D = error "unsafeRunOrr: D"

instance (Typeable v, Monoid v) => Semigroup (Orr v a) where
  D <> q = q
  p <> D = p
  Orr p <> Orr q = Orr $ do
    ((a, v, m), (n, v')) <- Syn (liftF (Or p q id))
    pure (a, V (foldV v), (m <> Orr n))

instance (Typeable v, Monoid v) => Monoid (Orr v a) where
  mempty = D

liftOrr :: Syn v a -> Orr v a
liftOrr p = Orr ((,E,D) <$> p)

-- isPure ----------------------------------------------------------------------

isPure :: Syn v a -> Maybe a
isPure (Syn (Pure a)) = Just a
isPure (Syn _) = Nothing

-- unblock ---------------------------------------------------------------------

unblock
  :: M.Map EventId EventValue
  -> Syn v a
  -> (Syn v a, Bool)

-- pure
unblock _ rsp@(Syn (Pure a)) = (rsp, False)

-- unblock
unblock m rsp@(Syn (Free (MapView f v next))) = (Syn (Free (MapView f v' next)), b)
  where
    (v', b) = unblock m v

-- forever
unblock _ rsp@(Syn (Free Forever)) = (rsp, False)

-- await
unblock m rsp@(Syn (Free (Await (Event eid') next)))
  = case M.lookup eid' m of
      Just (EventValue _ a) -> (Syn (next $ unsafeCoerce a), True)
      Nothing -> (rsp, False)

-- emit
unblock m rsp@(Syn (Free (Emit _ next))) = (Syn next, True)

-- and
unblock m rsp@(Syn (Free (And p q next)))
  = (Syn (Free (And p' q' next)), up || uq)
  where
    (p', up) = unblock m p
    (q', uq) = unblock m q

-- or
unblock m rsp@(Syn (Free (Or p q next)))
  = (Syn (Free (Or p' q' next)), up || uq)
  where
    (p', up) = unblock m p
    (q', uq) = unblock m q

-- advance ---------------------------------------------------------------------

data V v = E | V v | P (V v) (V v) | forall u. Monoid u => U (u -> v) (V u)

deriving instance Functor V

foldV :: Monoid v => V v -> v
foldV E = mempty
foldV (V v) = v
foldV (U f v) = f (foldV v)
foldV (P p q) = foldV p <> foldV q

-- advance . advance == advance
advance
  :: Monoid v
  => Int
  -> [IO ()]
  -> Syn v a
  -> V v
  -> (Int, [IO ()], Syn v a, V v)

-- pure
advance eid ios rsp@(Syn (Pure a)) v
  = (eid, ios, rsp, v)

-- forever
advance eid ios rsp@(Syn (Free Forever)) v
  = (eid, ios, rsp, v)

-- await
advance eid ios rsp@(Syn (Free (Await _ _))) v
  = (eid, ios, rsp, v)

-- view
advance eid ios rsp@(Syn (Free (View v next))) _
  = advance (eid + 1) ios (Syn next) (V v)

-- mapView
advance eid ios rsp@(Syn (Free (MapView f m next))) v
  = case rsp' of
      Syn (Pure a) -> advance eid' ios' (Syn $ next a) (V $ f (foldV v'))
      rsp' -> (eid', ios', Syn (Free (MapView f rsp' next)), U f v')
  where
    (eid', ios', rsp', v') = case v of
      U uf uv -> advance (eid + 1) ios m (unsafeCoerce uv)
      _ -> advance (eid + 1) ios m E

-- local
advance eid ios (Syn (Free (Local f next))) v
  = advance (eid + 1) ios (f (Event (Internal eid)) >>= Syn . next) v

-- emit
advance eid ios rsp@(Syn (Free (Emit _ _))) v
  = (eid, ios, rsp, v)

-- async
advance eid ios (Syn (Free (Async io next))) v
  = advance (eid + 1) (io:ios) (Syn next) v

-- and
advance eid ios rsp@(Syn (Free (And p q next))) v
  = case (isPure p', isPure q') of
      (Just a, Just b)
           -- TODO: should we foldV here? or use parallel Is?
        -> advance eid'' ios'' (Syn (next (a, b))) (V (foldV pv' <> foldV qv'))
      _ -> (eid'', ios'', Syn (Free (And p' q' next)), v')
  where
    v' = case (pv', qv') of
      (E, E) -> v
      _ -> P pv' qv'

    (pv, qv) = case v of
      P pv qv -> (pv, qv)
      _ -> (E, E)

    (eid', ios', p', pv') = advance (eid + 1) ios p pv
    (eid'', ios'', q', qv') = advance (eid' + 1) ios' q qv

advance eid ios rsp@(Syn (Free (Or p q next))) v
  = case (isPure p', isPure q') of
      (Just a, _)
        -> advance eid' ios' (Syn (next (a, (q', qv')))) (V (foldV pv'))
      (_, Just b)
        -> advance eid'' ios'' (Syn (next (b, (p', pv')))) (V (foldV qv'))
      _ -> (eid'', ios'', Syn (Free (Or p' q' next)), v')
  where
    (pv, qv) = case v of
      P pv qv -> (pv, qv)
      _ -> (E, E)

    v' = case (pv', qv') of
      (E, E) -> v
      (_, _) -> P pv' qv'

    (eid', ios', p', pv') = advance (eid + 1) ios p pv
    (eid'', ios'', q', qv') = advance (eid' + 1) ios' q qv

-- gather ----------------------------------------------------------------------

gather
  :: Syn v a
  -> M.Map EventId EventValue

-- pure
gather (Syn (Pure _)) = M.empty

-- mapview
gather (Syn (Free (MapView _ m next))) = gather m

-- forever
gather (Syn (Free Forever)) = M.empty

-- await
gather (Syn (Free (Await _ _))) = M.empty

-- emit
gather (Syn (Free (Emit e@(EventValue (Event ei) _) next))) = M.singleton ei e

-- and
gather (Syn (Free (And p q next))) = gather q <> gather p

-- or
gather (Syn (Free (Or p q next))) = gather q <> gather p

--------------------------------------------------------------------------------

stepOnce :: Monoid v => M.Map EventId EventValue -> Int -> Syn v a -> V v -> IO (Int, Syn v a, V v, [EventId], Bool)
stepOnce m' eid p v = do
  sequence_ ios
  pure (eid', p'', v', M.keys m, u)
  where
    (eid', ios, p', v') = advance eid [] p v
    m = gather p'
    (p'', u) = unblock (m' <> m) p'

stepAll :: Monoid v => M.Map EventId EventValue -> Int -> Syn v a -> V v -> IO (Either (Maybe a) (Int, Syn v a), V v, [([EventId], Syn v a)])
stepAll = go []
  where
    go es m eid p v = do
      (eid', p', v', eks, u) <- stepOnce m eid p v

      -- traceIO ("*** " <> show p)
      -- traceIO ("### " <> show p' <> ", EVENTS: " <> show (M.keys m) <> ", U: " <> show u)

      case (isPure p', u) of
        (Just a, _) -> pure (Left (Just a), v', (eks, p):es)
        (_, True) -> go ((eks ,p):es) M.empty eid' p' v'
        (_, False) -> pure (Right (eid', p'), v', (eks, p):es)

exhaust :: Typeable v => Monoid v => Syn v a -> IO (Maybe a, v)
exhaust p = do
  r <- stepAll M.empty 0 p E
  case r of
    (Left a, v, _)  -> pure (a, foldV v)
    (Right _, _, _) -> error "Blocked"

-- Pools -----------------------------------------------------------------------

data Pool v = Pool (Event Internal (Syn v ()))

pool :: Typeable v => Monoid v => (Pool v -> Syn v a) -> Syn v a
pool f = local $ \e -> go e E $ mconcat
  [ liftOrr (Right . Left <$> await e)
  , liftOrr (Left <$> f (Pool e))
  ]
  where
    go e v k = do
      (r, v', k') <- orr [ fromJust (runOrr k), view (foldV v) >> forever ]

      case r of
        Left a -> pure a
        Right (Left p)  -> go e v' $ mconcat
          [ liftOrr (Right . Left <$> await e)
          , liftOrr (fmap (Right . Right) p)
          , k'
          ]
        Right (Right _) -> go e v' k'

spawn :: Pool v -> Syn v () -> Syn v ()
spawn (Pool e) p = do
  emit e p

--------------------------------------------------------------------------------

{-# NOINLINE nextId #-}
nextId :: IORef Int
nextId = unsafePerformIO (newIORef 0)

newEvent :: IO (Event External b)
newEvent = Event . External <$> atomicModifyIORef' nextId (\eid -> (eid + 1, eid))

newtype Context v a = Context (MVar (Maybe (Int, Syn v a, V v)))

type Application v a r = (a -> IO (Context v r)) -> IO (Context v r)

run :: Syn v a -> IO (Context v a)
run p = Context <$> newMVar (Just (0, p, E))

push :: Typeable v => Monoid v => Context v b -> Event t a -> a -> IO (Maybe b, v)
push (Context v) e@(Event ei) a = modifyMVar v $ \v -> case v of
  Just (eid, p, v) -> do
    r <- stepAll (M.singleton ei (EventValue e a)) eid p v

    case r of
      (Left a, v, _) -> pure (Nothing, (a, foldV v))
      (Right (eid', p'), v', _) -> pure (Just (eid', p', v'), (Nothing, foldV v'))

  _ -> pure (Nothing, (Nothing, mempty))

event :: Application v (Event External a) r
event app = newEvent >>= app

--------------------------------------------------------------------------------

newTrail :: Monoid v => Context v a -> IO (Trail v a)
newTrail (Context ctx) = do
  pure $ Trail
    { trNotify  = undefined
    , trAdvance = modifyMVar ctx $ \ctx' -> case ctx' of
        Nothing -> pure (Nothing, (Nothing, E))
        Just (eid, p, v) -> do
          let (eid', ios, p', v') = advance eid [] p v
          sequence_ ios
          case p' of
            Syn (Pure a) -> pure (Just (eid', p', v'), (Just a, v'))
            _ -> pure (Just (eid', p', v'), (Nothing, v'))
    , trGather  = modifyMVar ctx $ \ctx' -> case ctx' of
        Nothing -> pure (Nothing, M.empty)
        Just (_, p, _) -> pure (ctx', gather p)
    , trUnblock = \m -> modifyMVar ctx $ \ctx' -> case ctx' of
        Nothing -> pure (Nothing, False)
        Just (eid, p, v) -> do
          let (p', u) = unblock m p
          pure (Just (eid, p', v), u)
    }
