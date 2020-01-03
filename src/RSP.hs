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

newtype StackLevel = StackLevel { getStackLevel :: Int }
  deriving (Eq, Ord, Num, Show)

data RSPF next
  = Async (IO ()) next

  | Forever

  | forall a b. Local (Event a -> RSP b) (b -> next)
  | Emit ExEvent next
  | forall a. Await (Event a) (a -> next)

  | forall a. Or (RSP a) (RSP a) ((a, RSP a) -> next)
  | forall a. And (RSP a) (RSP a) ([a] -> next)

  | Hole next
  | HoleNext next
  | forall a. Tag String (RSP a)

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
  show (RSP (Free (Tag s p))) = "<" <> s <> "> " <> show p
  show (RSP (Free (Hole _))) = "Hole"
  show (RSP (Free (HoleNext _))) = "HoleNext"

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

hole :: RSP ()
hole = RSP $ liftF (Hole ())

tag :: String -> RSP a -> RSP a
tag s p = RSP $ liftF (Tag s p)

data O a = O { advO :: RSP (a, O a) } | D
  deriving Show

emptyO :: O a
emptyO = D

singletonO :: RSP a -> O a
singletonO p = O $ do
  a <- p
  pure (a, D)

appendO :: O a -> O a -> O a
appendO D q = q
appendO p D = p
appendO (O p) (O q) = O $ do
  ((a, m), n) <- RSP (liftF (Or p q id))
  pure (a, m `appendO` O n)

orO :: RSP a -> O a -> O a
orO p D = O $ do
  r <- p
  pure (r, D)
orP p (O q) = O $ do
  ((a, k), u) <- RSP (liftF (Or p q id))
  undefined

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

testorr = run $ local $ \e -> local $ \f -> local $ \g -> do
  [_, Right (b, ks)] <- andd [ Left <$> (await e), Right <$> (orr' (ks e f g)) ]
  async (traceIO $ "KS1: " <> show (b, ks))
  [_, Right (b, ks)] <- andd [ Left <$> (await f >> await f), Right <$> (orr' ks) ]
  async (traceIO $ "KS2: " <> show (b, ks))
  [a, Right (b, ks)] <- andd [ Left <$> (await g >> await g >> await g), Right <$> (orr' ks) ]
  async (traceIO $ "KS3: " <> show (b, ks))
  pure a
  where
    ks e f g = [ emit e () >> pure 1, emit f () >> emit f () >> pure 2, emit g () >> emit g () >> emit g () >> pure 3 ]

testorr' = run $ local $ \e -> local $ \f -> local $ \g -> do
  [_, Right (b, ks)] <- andd [ Left <$> (await e), Right <$> (advO (ks e f g)) ]
  async (traceIO $ "KS1: " <> show (b, ks))
  [_, Right (b, ks)] <- andd [ Left <$> (await f >> await f), Right <$> (advO ks) ]
  async (traceIO $ "KS2: " <> show (b, ks))
  [a, Right (b, ks)] <- andd [ Left <$> (await g >> await g >> await g), Right <$> (advO ks) ]
  async (traceIO $ "KS3: " <> show (b, ks))
  pure a
  where
    ks e f g
      = singletonO (emit e () >> pure 1) `appendO`
        singletonO (emit f () >> emit f () >> pure 2) `appendO`
        singletonO (emit g () >> emit g () >> emit g () >> pure 3)

--------------------------------------------------------------------------------

bcast :: ExEvent -> RSP a -> RSP a
bcast e (RSP (Free (Tag s p)))
  = RSP (Free (Tag s (bcast e p)))
bcast (ExEvent (Event e) a) (RSP (Free (Await (Event e') next)))
  | e == e' = RSP (next $ unsafeCoerce a)
bcast e (RSP (Free (And p q next)))
  = RSP (Free (And (bcast e p) (bcast e q) next))
bcast e (RSP (Free (Or p q next)))
  = RSP (Free (Or (bcast e p) (bcast e q) next))
bcast _ p = p

isBlocked :: RSP a -> Bool
isBlocked (RSP (Pure _)) = True
isBlocked (RSP (Free (Await _ _))) = True
isBlocked (RSP (Free (And p q _))) = isBlocked p && isBlocked q
isBlocked (RSP (Free (Or p q _))) = isBlocked p && isBlocked q
isBlocked _ = False

-- step
--   :: M.Map EventId ExEvent
--   -> M.Map EventId ExEvent
--   -> EventIdInt
--   -> [IO ()]
--   -> RSP a
--   -> (M.Map EventId ExEvent, EventIdInt, [IO ()], RSP a, Bool)
-- 
-- -- pure
-- step em m eid ios rsp@(RSP (Pure a))
--   = (m, eid, ios, rsp, False)
-- 
-- -- local
-- step em m eid ios (RSP (Free (Local f next)))
--   = step em m (eid + 1) ios (f (Event (I eid)) >>= RSP . next)
-- 
-- -- emit
-- step em m eid ios (RSP (Free (Emit e@(ExEvent (Event eid') _) next)))
--   = step em (M.insert eid' e m) (eid + 1) ios (RSP next)
-- 
-- -- await
-- step em m eid ios rsp@(RSP (Free (Await (Event eid') next)))
--   = ( m
--     , eid
--     , ios
--     , case M.lookup eid' em of
--         Just (ExEvent _ a) -> RSP (next $ unsafeCoerce a)
--         Nothing -> rsp
--     , True
--     )
-- 
-- -- hole
-- step em m eid ios (RSP (Free (Hole next)))
--   = ( m
--     , eid
--     , ios
--     , RSP (Free (HoleNext next))
--     , True
--     )
-- 
-- -- hole
-- step em m eid ios (RSP (Free (HoleNext next)))
--   = step em m eid ios (RSP next)
-- 
-- -- async
-- step em m eid ios (RSP (Free (Async io next)))
--   = step em m (eid + 1) (io:ios) (RSP next)
-- 
-- -- and
-- step em m eid ios rsp@(RSP (Free (And p q next)))
--   = case (p', q') of
--       (RSP (Pure a), RSP (Pure b)) -> if pc || qc
--         then (m'', eid'', ios'', RSP (next [a, b]), pc || qc)
--         else step em m'' eid'' ios'' (RSP (next [a, b]))
--       _ -> (m'', eid'', ios'', RSP (Free (And p' q' next)), pc || qc)
--   where
--     (m', eid', ios', p', pc) = step em m (eid + 1) ios p
--     (m'', eid'', ios'', q', qc) = step em m' (eid' + 1) ios' q
-- 
-- -- or
-- step em m eid ios rsp@(RSP (Free (Or p q next)))
--   = case (p', q') of
--       (RSP (Pure a), _) -> if pc
--         then (m', eid', ios', RSP (next (a, q')), pc)
--         else step em m' eid' ios' (RSP (next (a, q')))
--       (_, RSP (Pure b)) -> if pc || qc
--         then (m'', eid'', ios'', RSP (next (b, p')), pc || qc)
--         else step em m'' eid'' ios'' (RSP (next (b, p')))
--       _ -> (m'', eid'', ios'', RSP (Free (Or p' q' next)), pc || qc)
--   where
--     (m', eid', ios', p', pc) = step em m (eid + 1) ios p
--     (m'', eid'', ios'', q', qc) = step em m' (eid' + 1) ios' q

unblock
  :: M.Map EventId ExEvent
  -> RSP a
  -> RSP a

-- pure
unblock _ rsp@(RSP (Pure a)) = rsp

-- await
unblock m rsp@(RSP (Free (Await (Event eid') next)))
  = case M.lookup eid' m of
      Just (ExEvent _ a) -> RSP (next $ unsafeCoerce a)
      Nothing -> rsp

-- and
unblock m rsp@(RSP (Free (And p q next)))
  = case (p', q') of
      (RSP (Pure a), RSP (Pure b))
        -> RSP (next [a, b])
      _ -> RSP (Free (And p' q' next))
  where
    p' = unblock m p
    q' = unblock m q

-- or
unblock m rsp@(RSP (Free (Or p q next)))
  = case (p', q') of
      (RSP (Pure a), _)
        -> RSP (next (a, q'))
      (_, RSP (Pure b))
        -> RSP (next (b, p'))
      _ -> RSP (Free (Or p' q' next))
  where
    p' = unblock m p
    q' = unblock m q

--------------------------------------------------------------------------------

gather
  :: M.Map EventId ExEvent
  -> EventIdInt
  -> [IO ()]
  -> RSP a
  -> (M.Map EventId ExEvent, EventIdInt, [IO ()], RSP a)

-- pure
gather m eid ios rsp@(RSP (Pure a))
  = (m, eid, ios, rsp)

-- await
gather m eid ios rsp@(RSP (Free (Await _ _)))
  = (m, eid, ios, rsp)

-- local
gather m eid ios (RSP (Free (Local f next)))
  = gather m (eid + 1) ios (f (Event (I eid)) >>= RSP . next)

-- emit
gather m eid ios (RSP (Free (Emit e@(ExEvent (Event ei) _) next)))
  = gather (M.insert ei e m) (eid + 1) ios (RSP next)

-- async
gather m eid ios (RSP (Free (Async io next)))
  = gather m (eid + 1) (io:ios) (RSP next)

-- and
gather m eid ios rsp@(RSP (Free (And p q next)))
  = case (p', q') of
      (RSP (Pure a), RSP (Pure b))
        -> gather m'' eid'' ios'' (RSP (next [a, b]))
      _ -> (m'', eid'', ios'', RSP (Free (And p' q' next)))
  where
    (m', eid', ios', p') = gather m (eid + 1) ios p
    (m'', eid'', ios'', q') = gather m' (eid' + 1) ios' q

-- or
gather m eid ios rsp@(RSP (Free (Or p q next)))
  = case (p', q') of
      (RSP (Pure a), _)
        -> gather m' eid' ios' (RSP (next (a, q')))
      (_, RSP (Pure b))
        -> gather m'' eid'' ios'' (RSP (next (b, p')))
      _ -> (m'', eid'', ios'', RSP (Free (Or p' q' next)))
  where
    (m', eid', ios', p') = gather m (eid + 1) ios p
    (m'', eid'', ios'', q') = gather m' (eid' + 1) ios' q

-- step
--   :: Maybe ExEvent
--   -> EventIdInt
--   -> RSP a
--   -> (Maybe ExEvent, EventIdInt, RSP a, IO ())
-- 
-- -- push
-- step (Just event) eid p
--   = (Nothing, eid + 1, bcast event p, pure ())
-- -- pop
-- step Nothing eid p
--   | isBlocked p = (Nothing, eid + 1, p, pure ())
-- 
-- -- local
-- step Nothing eid (RSP (Free (Local f next)))
--   = (Nothing, eid + 1, f (Event (I eid)) >>= RSP . next, pure ())
-- 
-- -- emit
-- step Nothing eid (RSP (Free (Emit e next)))
--   = (Just e, eid, RSP (next), pure ())
-- 
-- -- hole
-- step Nothing eid (RSP (Free (Hole next)))
--   = (Nothing, eid, RSP next, pure ())
-- 
-- -- async
-- step Nothing eid (RSP (Free (Async io next)))
--   = (Nothing, eid + 1, RSP next, io)
-- 
-- -- and-nop1
-- step Nothing eid (RSP (Free (And (RSP (Pure a)) q next)))
--   = (Nothing, eid + 1, q' a, pure ())
--   where
--    q' a = do
--      b <- q
--      RSP (next [a, b])
-- -- and-nop2
-- step Nothing eid (RSP (Free (And p (RSP (Pure b)) next)))
--   | isBlocked p = (Nothing, eid + 1, p' b, pure ())
--   where
--     p' b = do
--       a <- p
--       RSP (next [a, b])
-- -- and-adv2
-- step Nothing eid (RSP (Free (And p q next)))
--   | isBlocked p = (e, eid', RSP (Free (And p q' next)), io)
--   where
--     (e, eid', q', io) = step Nothing eid q
-- -- and-adv1
-- step Nothing eid (RSP (Free (And p q next)))
--   = (e, eid', RSP (Free (And p' q next)), io)
--   where
--     (e, eid', p', io) = step Nothing eid p
-- 
-- -- or-nop1
-- step Nothing eid (RSP (Free (Or (RSP (Pure a)) q next)))
--   = (Nothing, eid + 1, RSP (next (a, q)), pure ())
-- -- or-nop2
-- step Nothing eid (RSP (Free (Or p (RSP (Pure b)) next)))
--   = (Nothing, eid + 1, RSP (next (b, p)), pure ())
-- -- or-adv2
-- step Nothing eid (RSP (Free (Or p q next)))
--   | isBlocked p = (e, eid', RSP (Free (Or p q' next)), io)
--   where
--     (e, eid', q', io) = step Nothing eid q
-- -- or-adv1
-- step Nothing eid (RSP (Free (Or p q next)))
--   = (e, eid', RSP (Free (Or p' q next)), io)
--   where
--     (e, eid', p', io) = step Nothing eid p
-- 
-- step e eid p = error (isEvent e <> ", " <> show eid <> ", " <> show p)
--   where
--     isEvent (Just _) = "Event"
--     isEvent Nothing  = "No event"

--------------------------------------------------------------------------------

run :: RSP a -> IO a
run = go M.empty 0
  where
    go em 100 p = error "END"
    -- go em eid p = do
    --   traceIO ("*** " <> show p)
    --   let (em', eid', ios, p', _) = step em M.empty eid [] p
    --   traceIO ("### " <> show p' <> ", EVENTS: " <> show (M.keys em'))
    --   sequence_ ios
    --   case p' of
    --     RSP (Pure a) -> pure a
    --     _ -> if isBlocked p' && M.size em' == 0
    --       then error "Blocked"
    --       else go em' eid' p'

-- Pools -----------------------------------------------------------------------

data Pool = Pool (Event (RSP ()))

pool' :: Show a => (Pool -> RSP a) -> RSP a
pool' f = local (\e -> go e [Left <$> f (Pool e), Right . Left <$> await e])
  where
    go e ks = do
      (r, ks') <- orr' ks

      async $ traceIO ("POOL BEFORE: " <> show ks)
      async $ traceIO ("POOL AFTER : " <> show (r, ks'))
      
      case r of
        Left a          -> pure a
        Right (Left p)  -> go e $ concat
          [ [ Right . Left <$> await e ]
          , fmap (Right . Right) p:ks'
          ]
        Right (Right _) -> go e ks'

pool :: Show a => (Pool -> RSP a) -> RSP a
pool f = local $ \e -> go e
  (singletonO (Left <$> f (Pool e)) `appendO` singletonO (Right . Left <$> await e))
  where
    go e k = do
      async (traceIO "BEFORE")
      (r, k') <- advO k

      async (traceIO $ show (r, k'))

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
