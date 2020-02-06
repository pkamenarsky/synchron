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
import Control.Monad.Fix (fix)
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

newtype NodeId = NodeId Int deriving (Eq, Ord, Num, Show)

-- TODO: internal NodeId as well
data EventId = Internal (NodeId, Int) | External (NodeId, Int)
  deriving (Eq, Ord, Show)

data Internal
data External

data Event t a = Event (a -> a -> a) EventId

instance Show (Event t a) where
  show (Event _ e) = "Event " <> show e

data EventValue = forall t a. EventValue (Event t a) a

instance Show EventValue where
  show (EventValue e _) = show e

type Notify = M.Map EventId EventValue -> IO ()

data Trail v a = Trail
  { trAdvance :: (M.Map EventId EventValue -> IO ()) -> IO (Maybe a, V v)
  , trGather  :: IO (M.Map EventId EventValue)
  , trUnblock :: M.Map EventId EventValue -> IO Bool
  }

data SynF v next
  = Async (Notify -> IO ()) next

  | Forever

  | View v next

  | forall u a. Monoid u => MapView (u -> v) (Syn u a) (a -> next)

  | forall a. Remote (IO (Trail v a)) (a -> next)
  | forall a. RemoteU (Trail v a) (a -> next)

  | forall a b. Local (a -> a -> a) (Event Internal a -> Syn v b) (b -> next)
  | Emit EventValue next
  | forall t a. Await (Event t a) (a -> next)

  | forall t a. Reflect (v -> Maybe a -> IO ()) (Syn v a) (a -> next)

  | forall a b. Dyn (Event Internal [Syn v ()]) (Syn v b) [(Syn v (), V v)] (b -> next)

  | forall a. Or (Syn v a) (Syn v a) (a -> next)
  | forall a b. And (Syn v a) (Syn v b) ((a, b) -> next)

deriving instance Functor (SynF v)

newtype Syn v a = Syn { getSyn :: Free (SynF v) a }
  deriving (Functor, Applicative, Monad)

instance MonadFail (Syn v) where
  fail e = error e

instance Monoid v => Alternative (Syn v) where
  empty = view mempty >> forever
  a <|> b = orr [a, b]

color :: Int -> String -> String
color c s = "\ESC[" <> show (31 + (c `mod` 7)) <> "m" <> s <> "\ESC[m"

evColor :: EventId -> String -> String
evColor (Internal (_, c)) = color c
evColor (External (_, c)) = color c

instance Show (Syn v a) where
  show (Syn (Pure a)) = "◆"
  show (Syn (Free (Async e _))) = "A"
  show (Syn (Free Forever)) = "∞"
  show (Syn (Free (Dyn _ p ps _))) = "dyn (" <> show p <> ") [" <> intercalate ", " (map (show . fst) ps) <> "]"
  show (Syn (Free (MapView _ m _))) = "fmap (" <> show m <> ")"
  show (Syn (Free (Remote _ _))) = "remote"
  show (Syn (Free (RemoteU _ _))) = "remote"
  show (Syn (Free (View _ _))) = "V"
  show (Syn (Free (Local _ _ _))) = "local"
  show (Syn (Free (Emit (EventValue (Event _ e) _) _))) = evColor e "▲"
  show (Syn (Free (Await (Event _ e) _))) = evColor e "○"
  show (Syn (Free (Or a b _))) = "∨ [" <> intercalate ", " (map show [a, b]) <> "]"
  show (Syn (Free (And a b _))) = "∧ [" <> show a <> ", " <> show b <> "]"

data DbgBinOp = DbgAnd | DbgOr deriving (Eq, Show)

data DbgSyn
  = DbgDone
  | DbgBlocked
  | DbgForever
  | DbgAwait EventId DbgSyn
  | DbgEmit EventId DbgSyn
  | DbgJoin DbgSyn
  | DbgBin DbgBinOp (DbgSyn -> DbgSyn) (DbgSyn -> DbgSyn) DbgSyn

mapView :: Monoid u => (u -> v) -> Syn u a -> Syn v a
mapView f m = Syn $ liftF (MapView f m id)

remote :: IO (Trail v a) -> Syn v a
remote trail = Syn $ liftF (Remote trail id)

async :: (Notify -> IO ()) -> Syn v ()
async io = Syn $ liftF (Async io ())

forever :: Syn v a
forever = Syn $ liftF Forever

view :: v -> Syn v ()
view v = Syn $ liftF (View v ())

local :: (Event Internal a -> Syn v b) -> Syn v b
local f = Syn $ liftF (Local const f id)

local' :: (a -> a -> a) -> (Event Internal a -> Syn v b) -> Syn v b
local' conc f = Syn $ liftF (Local conc f id)

emit :: Event Internal a -> a -> Syn v ()
emit e a = Syn $ liftF (Emit (EventValue e a) ())

emitValue :: EventValue -> Syn v ()
emitValue (EventValue e a) = Syn $ liftF (Emit (EventValue e a) ())

await :: Event t a -> Syn v a
await e = Syn $ liftF (Await e id)

-- | Left biased.
orr :: Monoid v => [Syn v a] -> Syn v a
orr [a] = a
orr [a, b] = Syn $ liftF (Or a b id)
orr (a:as) = orr [a, orr as]

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

-- unblock ---------------------------------------------------------------------

unblock
  :: M.Map EventId EventValue
  -> Syn v a
  -> (Syn v a, Bool)

-- pure
unblock _ rsp@(Syn (Pure a)) = (rsp, False)

-- local
unblock _ rsp@(Syn (Free (Local _ _ _))) = (rsp, False)

-- mapView
unblock m rsp@(Syn (Free (MapView f v next))) = (Syn (Free (MapView f v' next)), b)
  where
    (v', b) = unblock m v

-- forever
unblock _ rsp@(Syn (Free Forever)) = (rsp, False)

-- await
unblock m rsp@(Syn (Free (Await (Event _ eid') next)))
  = case M.lookup eid' m of
      Just (EventValue _ a) -> (Syn (next $ unsafeCoerce a), True)
      Nothing -> (rsp, False)

-- dyn
unblock m rsp@(Syn (Free (Dyn e@(Event _ eid') p ps next)))
  = (Syn (Free (Dyn e p' (zip (map fst ps') (map snd ps) <> map (,E) newPs) next)), u || or (map snd ps'))
  where
    newPs = case M.lookup eid' m of
              Just (EventValue _ ps) -> unsafeCoerce ps
              Nothing -> []

    (p', u) = unblock m p
    ps' = map (unblock m) (map fst ps)

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

unblockIO
  :: M.Map EventId EventValue
  -> Syn v a
  -> IO (Syn v a, Bool)

-- remote
unblockIO m rsp@(Syn (Free (RemoteU trail next))) = do
  u <- trUnblock trail m
  pure (rsp, u)

-- pure
unblockIO _ rsp@(Syn (Pure a)) = pure (rsp, False)

-- local
unblockIO _ rsp@(Syn (Free (Local _ _ _))) = pure (rsp, False)

-- mapView
unblockIO m rsp@(Syn (Free (MapView f v next))) = do
  (v', b) <- unblockIO m v
  pure (Syn (Free (MapView f v' next)), b)

-- forever
unblockIO _ rsp@(Syn (Free Forever)) = pure (rsp, False)

-- await
unblockIO m rsp@(Syn (Free (Await (Event _ eid') next)))
  = case M.lookup eid' m of
      Just (EventValue _ a) -> pure (Syn (next $ unsafeCoerce a), True)
      Nothing -> pure (rsp, False)

-- dyn
unblockIO m rsp@(Syn (Free (Dyn e@(Event _ eid') p ps next))) = do
  (p', u) <- unblockIO m p
  ps' <- traverse (unblockIO m) (map fst ps)
  pure (Syn (Free (Dyn e p' (zip (map fst ps') (map snd ps) <> map (,E) newPs) next)), u || or (map snd ps'))
  where
    newPs = case M.lookup eid' m of
              Just (EventValue _ ps) -> unsafeCoerce ps
              Nothing -> []

-- emit
unblockIO m rsp@(Syn (Free (Emit _ next))) = pure (Syn next, True)

-- and
unblockIO m rsp@(Syn (Free (And p q next))) = do
  (p', up) <- unblockIO m p
  (q', uq) <- unblockIO m q

  pure (Syn (Free (And p' q' next)), up || uq)

-- or
unblockIO m rsp@(Syn (Free (Or p q next))) = do
  (p', up) <- unblockIO m p
  (q', uq) <- unblockIO m q
  pure (Syn (Free (Or p' q' next)), up || uq)

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
  => NodeId
  -> Int
  -> [IO ()]
  -> Syn v a
  -> V v
  -> (Int, [IO ()], Syn v a, DbgSyn -> DbgSyn, V v)

-- pure
advance nid eid ios rsp@(Syn (Pure a)) v
  = (eid, ios, rsp, \_ -> DbgDone, v)

-- forever
advance nid eid ios rsp@(Syn (Free Forever)) v
  = (eid, ios, rsp, \_ -> DbgForever, v)

-- await
advance nid eid ios rsp@(Syn (Free (Await (Event _ e) _))) v
  = (eid, ios, rsp, \dnext -> DbgAwait e dnext, v)

-- view
advance nid eid ios rsp@(Syn (Free (View v next))) _
  = advance nid eid ios (Syn next) (V v)

-- mapView
advance nid eid ios rsp@(Syn (Free (MapView f m next))) v
  = case rsp' of
      Syn (Pure a) -> advance nid eid' ios' (Syn $ next a) (V $ f (foldV v'))
      rsp' -> (eid', ios', Syn (Free (MapView f rsp' next)), dbg', U f v')
  where
    (eid', ios', rsp', dbg', v') = case v of
      U uf uv -> advance nid eid ios m (unsafeCoerce uv)
      _ -> advance nid eid ios m E

-- local
advance nid eid ios (Syn (Free (Local conc f next))) v
  = advance nid (eid + 1) ios (f (Event conc (Internal (nid, eid))) >>= Syn . next) v

-- dyn
advance nid eid ios (Syn (Free (Dyn e p ps next))) v
  = case p' of
      Syn (Pure a) -> advance nid eid' ios' (Syn (next a)) $ case v' of
        P pv _ -> pv
        v' -> v'
      otherwise -> (eid'', ios'', Syn (Free (Dyn e p' ps' next)), \_ -> DbgDone, v'') -- TODO: DbgDone
  where
    pv = case v of
      E -> E
      P pv _ -> pv

    (eid', ios', p', dbg', v') = advance nid eid ios p pv
    (eid'', ios'', ps', dbg'', v'') = go [] eid' ios' ps

    go rps eid ios [] = (eid, ios, map fst (reverse rps), \_ -> DbgDone, P v' (V $ foldMap foldV (map (snd . fst) (reverse rps))))
    go rps eid ios ((p, v):ps) = case advance nid eid ios p v of
      (eid', ios', Syn (Pure a), dbg', v') -> go rps eid' ios' ps
      (eid', ios', p', dbg', v') -> go (((p', v'), dbg'):rps) eid' ios' ps

-- emit
advance nid eid ios rsp@(Syn (Free (Emit (EventValue (Event _ e) _) _))) v
  = (eid, ios, rsp, \dbg -> DbgEmit e dbg, v)

-- async
advance nid eid ios (Syn (Free (Async io next))) v
  = advance nid eid (io undefined:ios) (Syn next) v

-- and
advance nid eid ios rsp@(Syn (Free (And p q next))) v
  = case (p', q') of
      (Syn (Pure a), Syn (Pure b))
        -> mapDbg (\fd dbg -> DbgJoin (fd dbg)) $ advance nid eid'' ios'' (Syn (next (a, b))) (V (foldV pv' <> foldV qv'))
      _ -> (eid'', ios'', Syn (Free (And p' q' next)), dbgcomp, v')
  where
    dbgcomp (DbgBin op pd qd nd) = DbgBin op (pdbg' . pd) (qdbg' . qd) nd
    dbgcomp p = DbgBin DbgAnd pdbg' qdbg' p

    v' = case (pv', qv') of
      (E, E) -> v
      _ -> P pv' qv'

    (pv, qv) = case v of
      P pv qv -> (pv, qv)
      _ -> (E, E)

    (eid', ios', p', pdbg', pv') = advance nid eid ios p pv
    (eid'', ios'', q', qdbg', qv') = advance nid eid' ios' q qv

advance nid eid ios rsp@(Syn (Free (Or p q next))) v
  = case (p', q') of
      (Syn (Pure a), _)
        -> mapDbg (\fd dbg -> DbgJoin (fd dbg)) $ advance nid eid' ios' (Syn (next a)) (V (foldV pv' <> foldV qv'))
      (_, Syn (Pure b))
        -> mapDbg (\fd dbg -> DbgJoin (fd dbg)) $ advance nid eid'' ios'' (Syn (next b)) (V (foldV pv' <> foldV qv'))
      _ -> (eid'', ios'', Syn (Free (Or p' q' next)), dbgcomp, v')
  where
    dbgcomp (DbgBin op pd qd nd) = DbgBin op (pdbg' . pd) (qdbg' . qd) nd
    dbgcomp (DbgJoin p) = DbgJoin (DbgBin DbgOr pdbg' qdbg' p)
    dbgcomp p = DbgBin DbgOr pdbg' qdbg' p

    (pv, qv) = case v of
      P pv qv -> (pv, qv)
      _ -> (E, E)

    v' = case (pv', qv') of
      (E, E) -> v
      (_, _) -> P pv' qv'

    (eid', ios', p', pdbg', pv') = advance nid eid ios p pv
    (eid'', ios'', q', qdbg', qv') = advance nid eid' ios' q qv

advance nid eid ios p v = error (show p)

mapDbg
  :: ((DbgSyn -> DbgSyn) -> DbgSyn -> DbgSyn)
  -> (Int, [IO ()], Syn v a, DbgSyn -> DbgSyn, V v)
  -> (Int, [IO ()], Syn v a, DbgSyn -> DbgSyn, V v)
mapDbg f (eid, ios, p, dbg, v) = (eid, ios, p, f dbg, v)

advanceIO
  :: Monoid v
  => NodeId
  -> Int
  -> [IO ()]
  -> Notify
  -> Syn v a
  -> V v
  -> IO (Int, [IO ()], Syn v a, DbgSyn -> DbgSyn, V v)

-- remote
advanceIO nid eid ios notify (Syn (Free (Remote make next))) v = do
  trail <- make
  advanceIO nid eid ios notify (Syn (Free (RemoteU trail next))) v

-- remoteU
advanceIO nid eid ios notify rsp@(Syn (Free (RemoteU trail next))) _ = do
  r <- trAdvance trail notify
  case r of
    (Just a, v') -> advanceIO nid eid ios notify (Syn $ next a) v'
    (Nothing, v') -> pure (eid, ios, rsp, id, v')

-- pure
advanceIO nid eid ios notify rsp@(Syn (Pure a)) v
  = pure (eid, ios, rsp, \_ -> DbgDone, v)

-- forever
advanceIO nid eid ios notify rsp@(Syn (Free Forever)) v
  = pure (eid, ios, rsp, \_ -> DbgForever, v)

-- await
advanceIO nid eid ios notify rsp@(Syn (Free (Await (Event _ e) _))) v
  = pure (eid, ios, rsp, \dnext -> DbgAwait e dnext, v)

-- view
advanceIO nid eid ios notify rsp@(Syn (Free (View v next))) _
  = advanceIO nid eid ios notify (Syn next) (V v)

-- mapView
advanceIO nid eid ios notify rsp@(Syn (Free (MapView f m next))) v = do
  (eid', ios', rsp', dbg', v') <- case v of
    U uf uv -> advanceIO nid eid ios notify m (unsafeCoerce uv)
    _ -> advanceIO nid eid ios notify m E

  case rsp' of
    Syn (Pure a) -> advanceIO nid eid' ios' notify (Syn $ next a) (V $ f (foldV v'))
    rsp' -> pure (eid', ios', Syn (Free (MapView f rsp' next)), dbg', U f v')
  where

-- local
advanceIO nid eid ios notify (Syn (Free (Local conc f next))) v
  = advanceIO nid (eid + 1) ios notify (f (Event conc (Internal (nid, eid))) >>= Syn . next) v

-- dyn
advanceIO nid eid ios notify (Syn (Free (Dyn e p ps next))) v = do
  let pv = case v of
        E -> E
        P pv _ -> pv
  
  (eid', ios', p', dbg', v') <- advanceIO nid eid ios notify p pv

  let go rps eid ios [] = pure (eid, ios, map fst (reverse rps), \_ -> DbgDone, P v' (V $ foldMap foldV (map (snd . fst) (reverse rps))))
      go rps eid ios ((p, v):ps) = do
        r <- advanceIO nid eid ios notify p v
        case r of
          (eid', ios', Syn (Pure a), dbg', v') -> go rps eid' ios' ps
          (eid', ios', p', dbg', v') -> go (((p', v'), dbg'):rps) eid' ios' ps

  (eid'', ios'', ps', dbg'', v'') <- go [] eid' ios' ps

  case p' of
    Syn (Pure a) -> advanceIO nid eid' ios' notify (Syn (next a)) $ case v' of
      P pv _ -> pv
      v' -> v'
    otherwise -> pure (eid'', ios'', Syn (Free (Dyn e p' ps' next)), \_ -> DbgDone, v'') -- TODO: DbgDone

-- emit
advanceIO nid eid ios notify rsp@(Syn (Free (Emit (EventValue (Event _ e) _) _))) v
  = pure (eid, ios, rsp, \dbg -> DbgEmit e dbg, v)

-- async
advanceIO nid eid ios notify (Syn (Free (Async io next))) v
  = advanceIO nid eid (io notify:ios) notify (Syn next) v

-- and
advanceIO nid eid ios notify rsp@(Syn (Free (And p q next))) v = do
  let (pv, qv) = case v of
        P pv qv -> (pv, qv)
        _ -> (E, E)

  (eid', ios', p', pdbg', pv') <- advanceIO nid eid ios notify p pv
  (eid'', ios'', q', qdbg', qv') <- advanceIO nid eid' ios' notify q qv

  let dbgcomp (DbgBin op pd qd nd) = DbgBin op (pdbg' . pd) (qdbg' . qd) nd
      dbgcomp p = DbgBin DbgAnd pdbg' qdbg' p

      v' = case (pv', qv') of
        (E, E) -> v
        _ -> P pv' qv'

  case (p', q') of
    (Syn (Pure a), Syn (Pure b))
      -> mapDbg (\fd dbg -> DbgJoin (fd dbg)) <$> advanceIO nid eid'' ios'' notify (Syn (next (a, b))) (V (foldV pv' <> foldV qv'))
    _ -> pure (eid'', ios'', Syn (Free (And p' q' next)), dbgcomp, v')

advanceIO nid eid ios notify rsp@(Syn (Free (Or p q next))) v = do
  let (pv, qv) = case v of
        P pv qv -> (pv, qv)
        _ -> (E, E)

  (eid', ios', p', pdbg', pv') <- advanceIO nid eid ios notify p pv
  (eid'', ios'', q', qdbg', qv') <- advanceIO nid eid' ios' notify q qv

  let dbgcomp (DbgBin op pd qd nd) = DbgBin op (pdbg' . pd) (qdbg' . qd) nd
      dbgcomp (DbgJoin p) = DbgJoin (DbgBin DbgOr pdbg' qdbg' p)
      dbgcomp p = DbgBin DbgOr pdbg' qdbg' p

      v' = case (pv', qv') of
        (E, E) -> v
        (_, _) -> P pv' qv'

  case (p', q') of
    (Syn (Pure a), _)
      -> mapDbg (\fd dbg -> DbgJoin (fd dbg)) <$> advanceIO nid eid' ios' notify (Syn (next a)) (V (foldV pv' <> foldV qv'))
    (_, Syn (Pure b))
      -> mapDbg (\fd dbg -> DbgJoin (fd dbg)) <$> advanceIO nid eid'' ios'' notify (Syn (next b)) (V (foldV pv' <> foldV qv'))
    _ -> pure (eid'', ios'', Syn (Free (Or p' q' next)), dbgcomp, v')

-- gather ----------------------------------------------------------------------

-- TODO: MonoidMap
concatEventValues :: EventValue -> EventValue -> EventValue
concatEventValues (EventValue e@(Event conc _) a) (EventValue _ b) = EventValue e (a `conc` unsafeCoerce b)

gather
  :: Syn v a
  -> M.Map EventId EventValue

-- pure
gather (Syn (Pure _)) = M.empty

-- local
gather (Syn (Free (Local _ _ _))) = M.empty

-- mapview
gather (Syn (Free (MapView _ m next))) = gather m

-- forever
gather (Syn (Free Forever)) = M.empty

-- await
gather (Syn (Free (Await _ _))) = M.empty

-- emit
gather (Syn (Free (Emit e@(EventValue (Event _ ei) _) next))) = M.singleton ei e

-- dyn
gather (Syn (Free (Dyn _ p ps _))) = M.unionWith concatEventValues (gather p) (M.unionsWith concatEventValues (map gather (map fst ps)))

-- and
gather (Syn (Free (And p q next))) = M.unionWith concatEventValues (gather q) (gather p)

-- or
gather (Syn (Free (Or p q next))) = M.unionWith concatEventValues (gather q) (gather p)

gatherIO
  :: Syn v a
  -> IO (M.Map EventId EventValue)

-- remote
gatherIO (Syn (Free (RemoteU trail _))) = trGather trail

-- pure
gatherIO (Syn (Pure _)) = pure M.empty

-- local
gatherIO (Syn (Free (Local _ _ _))) = pure M.empty

-- mapview
gatherIO (Syn (Free (MapView _ m next))) = gatherIO m

-- forever
gatherIO (Syn (Free Forever)) = pure M.empty

-- await
gatherIO (Syn (Free (Await _ _))) = pure M.empty

-- emit
gatherIO (Syn (Free (Emit e@(EventValue (Event _ ei) _) next))) = pure (M.singleton ei e)

-- dyn
gatherIO (Syn (Free (Dyn _ p ps _))) =
  M.unionWith concatEventValues <$> gatherIO p <*> (M.unionsWith concatEventValues <$> (traverse gatherIO (map fst ps)))

-- and
gatherIO (Syn (Free (And p q next))) = M.unionWith concatEventValues <$> gatherIO q <*> gatherIO p

-- or
gatherIO (Syn (Free (Or p q next))) = M.unionWith concatEventValues <$> gatherIO q <*> gatherIO p

--------------------------------------------------------------------------------

stepOnce :: Monoid v => M.Map EventId EventValue -> NodeId -> Int -> Syn v a -> V v -> IO (Int, Syn v a, V v, [EventId], Bool)
stepOnce m' nid eid p v = do
  (eid', ios, p', _, v') <- advanceIO nid eid [] (const $ pure ()) p v
  m <- gatherIO p'
  (p'', u) <- unblockIO (m' <> m) p'
  sequence_ ios
  pure (eid', p'', v', M.keys (m' <> m), u)

stepOnce' :: Monoid v => M.Map EventId EventValue -> NodeId -> Int -> Syn v a -> V v -> (Int, Syn v a, DbgSyn -> DbgSyn, V v, M.Map EventId EventValue, [IO ()], Bool)
stepOnce' m nid eid p v = (eid', p'', dbg, v', m', ios, u)
  where
    (p', u) = unblock m p
    (eid', ios, p'', dbg, v') = advance nid eid [] p' v
    m' = gather p''

stepAll :: Monoid v => [M.Map EventId EventValue] -> NodeId -> Int -> Syn v a -> V v -> IO (Either (Maybe a) (Int, Syn v a), V v, [([EventId], Syn v a)])
stepAll = go []
  where
    go es ms' nid eid p v = do
      let (m, ms) = case ms' of
            [] -> (M.empty, [])
            (m':ms') -> (m', ms')

      (eid', p', v', eks, u) <- stepOnce m nid eid p v

      -- traceIO ("> " <> show p <> ", Events: " <> intercalate "," (map (flip evColor "▲") eks) <> ", U: " <> show u)
      -- traceIO ("< " <> show p')
      -- traceIO ""

      case (p', u) of
        (Syn (Pure a), _) -> pure (Left (Just a), v', (eks, p'):es)
        (_, True) -> go ((eks, p'):es) ms nid eid' p' v'
        (_, False) -> pure (Right (eid', p'), v', (eks, p'):es)

stepAllTrail :: Monoid v => [M.Map EventId EventValue] -> Notify -> Trail v a -> IO ()
stepAllTrail ms notify trail = go ms
  where
    go [] = do
      trAdvance trail notify
      m <- trGather trail
      if M.size m > 0
        then do
          trUnblock trail m
          go []
        else do
          trAdvance trail notify
          pure ()
    go (m':ms') = do
      trAdvance trail notify
      m <- trGather trail
      trUnblock trail (m' <> m)
      go ms'

stepAll' :: Monoid v => M.Map EventId EventValue -> NodeId -> Int -> Syn v a -> V v -> IO (Either (Maybe a) (Syn v a), Int, [([EventId], Syn v a)])
stepAll' = go []
  where
    go es m nid eid p v = do
      (eid', p', v', eks, u) <- stepOnce m nid eid p v

      -- traceIO ("> " <> show p <> ", Events: " <> intercalate "," (map (flip evColor "▲") eks) <> ", U: " <> show u)
      -- traceIO ("< " <> show p')
      -- traceIO ""

      case (p', u) of
        (Syn (Pure a), _) -> pure (Left (Just a), eid', (eks, p):es)
        (_, True) -> go ((eks, p):es) M.empty nid eid' p' v'
        (_, False) -> pure (Right p', eid', (eks, p):es)

exhaust :: Typeable v => Monoid v => NodeId -> Syn v a -> IO (Maybe a, v)
exhaust nid p = do
  r <- stepAll [] nid 0 p E
  case r of
    (Left a, v, _)  -> pure (a, foldV v)
    (Right _, _, _) -> error "Blocked"

-- Pools -----------------------------------------------------------------------

data Pool v = Pool (Event Internal [Syn v ()])

pool :: Monoid v => (Pool v -> Syn v a) -> Syn v a
pool f = local' (<>) $ \e -> Syn (liftF (Dyn e (f (Pool e)) [] id))

spawn :: Pool v -> Syn v () -> Syn v ()
spawn (Pool e) p = emit e [p]

data Connection = Connection
  { connNotify :: M.Map EventId EventValue
  }

share :: Connection -> Pool v -> Syn v () -> Syn v ()
share = undefined

--------------------------------------------------------------------------------

{-# NOINLINE nextId #-}
nextId :: IORef Int
nextId = unsafePerformIO (newIORef 0)

newEvent :: NodeId -> IO (Event t a)
newEvent nid = Event const . External <$> atomicModifyIORef' nextId (\eid -> (eid + 1, (nid, eid)))

newEvent' :: (a -> a -> a) -> NodeId -> IO (Event t a)
newEvent' conc nid = Event conc . External <$> atomicModifyIORef' nextId (\eid -> (eid + 1, (nid, eid)))

data Context v a = Context NodeId (MVar (Maybe (Int, Syn v a, V v)))

type Application v a r = (a -> IO (Context v r)) -> IO (Context v r)

run :: NodeId -> Syn v a -> IO (Context v a)
run nid p = Context nid <$> newMVar (Just (0, p, E))

push :: Monoid v => Context v b -> Event t a -> a -> IO (Maybe b, v)
push (Context nid v) (Event conc ei) a = modifyMVar v $ \v -> case v of
  Just (eid, p, v) -> do
    r <- stepAll [M.singleton ei (EventValue (Event conc ei) a)] nid eid p v

    case r of
      (Left a, v', _) -> pure (Nothing, (a, foldV v'))
      (Right (eid', p'), v', _) -> pure (Just (eid', p', v'), (Nothing, foldV v'))

  _ -> pure (Nothing, (Nothing, mempty))

event :: NodeId -> Application v (Event External a) r
event nid app = newEvent nid >>= app

--------------------------------------------------------------------------------

newTrail
  :: Monoid v
  => NodeId
  -> Syn v a
  -> IO (Trail v a)
newTrail nid p = do
  Context _ ctx <- run nid p
  pure $ Trail
    { trAdvance = \notify -> modifyMVar ctx $ \ctx' -> case ctx' of
        Nothing -> pure (Nothing, (Nothing, E))
        Just (eid, p, v) -> do
          (eid', ios, p', _, v') <- advanceIO nid eid [] notify p v
          sequence_ ios
          case p' of
            Syn (Pure a) -> pure (Just (eid', p', v'), (Just a, v'))
            _ -> pure (Just (eid', p', v'), (Nothing, v'))
    , trGather  = modifyMVar ctx $ \ctx' -> case ctx' of
        Nothing -> pure (Nothing, M.empty)
        Just (_, p, _) -> (ctx',) <$> gatherIO p
    , trUnblock = \m -> modifyMVar ctx $ \ctx' -> case ctx' of
        Nothing -> pure (Nothing, False)
        Just (eid, p, v) -> do
          (p', u) <- unblockIO m p
          pure (Just (eid, p', v), u)
    }

runTrail :: Monoid v => Notify -> Trail v a -> IO ()
runTrail = stepAllTrail []

--------------------------------------------------------------------------------

data Mealy v a b = Mealy { runMealy :: a -> Syn v (a, Mealy v a b) } | Done b

viewM :: Monoid v => [Mealy v a b] -> Mealy v [a] b
viewM ms = go ms
  where
    replace i a as = take i as <> [a] <> drop (i + 1) as

    go ms = Mealy $ \as -> do
      ((a, next), i) <- orr [ (,i) <$> runMealy m a | (i, (m, a)) <- zip [0..] (zip ms as) ]
      pure (replace i a as,  go (replace i next ms))
