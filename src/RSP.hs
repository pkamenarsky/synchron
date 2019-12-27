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
import Data.Maybe (isJust, listToMaybe, mapMaybe)

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

  | forall a. OrU (RSP a) (RSP a) ((a, RSP a) -> next)
  | forall a. AndU (RSP a) (RSP a) ([a] -> next)

  | forall a. Canrun StackLevel next
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
  show (RSP (Free (OrU a b _)))  = "OrU [" <> intercalate ", " (map show [a, b]) <> "]"
  show (RSP (Free (AndU a b _))) = "AndU [" <> intercalate ", " (map show [a, b]) <> "]"
  show (RSP (Free (Canrun (StackLevel n) p))) = "Canrun " <> show n <> " (" <> show (RSP p) <> ")"
  show (RSP (Free (Tag s p))) = "<" <> s <> "> " <> show p

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
bcast e (RSP (Free (AndU p q next)))
  = RSP (Free (AndU (bcast e p) (bcast e q) next))
-- TODO
-- bcast e (RSP (Free (And p q next)))
--   = RSP (Free (And (bcast e p) (bcast e q) next))
bcast e (RSP (Free (OrU p q next)))
  = RSP (Free (OrU (bcast e p) (bcast e q) next))
-- TODO
-- bcast e (RSP (Free (Or p q next)))
--   = RSP (Free (Or (bcast e p) (bcast e q) next))
-- TODO
-- bcast e (RSP (Free (Canrun n next)))
--   = RSP (Free (Canrun n (getRSP $ bcast e (RSP next))))
bcast _ p = p

isBlocked :: RSP a -> StackLevel -> Bool
isBlocked (RSP (Free (Tag _ p))) n = isBlocked p n
isBlocked (RSP (Free (Await _ _))) _ = True
isBlocked (RSP (Free (Canrun m _))) n = n > m
isBlocked (RSP (Free (AndU p q _))) n = isBlocked p n && isBlocked q n
isBlocked (RSP (Free (OrU p q _))) n = isBlocked p n && isBlocked q n
isBlocked _ _ = False

isBlocked' :: RSP a -> StackLevel -> Bool
isBlocked' (RSP (Free (Tag _ p))) n = isBlocked' p n
isBlocked' (RSP (Free (Await _ _))) _ = True
isBlocked' (RSP (Free (Canrun m _))) n = n > m
isBlocked' (RSP (Free (And p q _))) n = isBlocked' p n && isBlocked' q n
isBlocked' (RSP (Free (Or p q _))) n = isBlocked' p n && isBlocked' q n
isBlocked' _ _ = False

step
  :: Maybe ExEvent
  -> StackLevel
  -> EventIdInt
  -> RSP a
  -> (Maybe ExEvent, StackLevel, EventIdInt, RSP a, IO ())

step e eid n (RSP (Free (Tag s p))) = (e', n', eid', (RSP $ Free $ Tag s p'), io)
  where
     (e', n', eid', p', io) = step e eid n p

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
step Nothing n eid (RSP (Free (And (RSP p) (RSP q) next)))
  | isBlocked' (RSP p) n = (Nothing, n, eid + 1, RSP (Free (AndU (RSP p) (RSP (Free (Canrun n q))) next)), pure ())
  | otherwise = (Nothing, n, eid + 1, RSP (Free (AndU (RSP q) (RSP (Free (Canrun n p))) next)), pure ())
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
step Nothing n eid (RSP (Free (Or (RSP p) (RSP q) next)))
  | isBlocked' (RSP p) n = (Nothing, n, eid + 1, RSP (Free (OrU (RSP p) (RSP (Free (Canrun n q))) next)), pure ())
  | otherwise = (Nothing, n, eid + 1, RSP (Free (OrU (RSP q) (RSP (Free (Canrun n p))) next)), pure ())
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
    -- go e n 100 p = error "END"
    go e n eid p = do
      traceIO (show (getStackLevel n) <> ": " <> show p <> ", " <> show (isJust e))
      let (e', n', eid', p', io) = step e n eid p
      io
      case p' of
        RSP (Pure a) -> pure a
        _ -> go e' n' eid' p'

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
      (r, k') <- advO k

      case r of
        Left a -> pure a
        Right (Left p)  -> go e
          ( (singletonO (Right . Left <$> await e))
            `appendO`
            (singletonO (fmap (Right . Right) p))
            `appendO` k'
          )
        Right (Right _) -> go e k'

spawn :: Pool -> RSP () -> RSP ()
spawn (Pool e) p = emit e p
