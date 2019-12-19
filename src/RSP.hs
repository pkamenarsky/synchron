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

import Data.IORef
import Data.Maybe (isJust)

import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Debug.Trace


newtype Context = Context (MVar (Maybe (RSP ())))

newtype Event a = Event (IORef (Maybe a))

newEvent :: IO (Event a)
newEvent = Event <$> newIORef Nothing

emit :: Context -> Event a -> a -> IO Bool
emit (Context ctx) e@(Event event) a = do
  modifyMVar ctx $ \rsp' -> do
    writeIORef event (Just a)
    case rsp' of
      Nothing -> pure (Nothing, False)
      Just rsp -> (,True) <$> go [] (Right e) rsp
  where
    go ks e rsp = do
      a <- runRSP e rsp
      case a of
        Done _ -> do
          writeIORef event Nothing
          pure Nothing
        Blocked rsp' -> case ks of
          [] -> do
            writeIORef event Nothing
            pure (Just rsp')
          ((e', k):ks) -> go ks e' (k rsp)
        Next rsp' -> do
          go ks e rsp'
        Cont e' b k -> go ((e, k):ks) (Left (VEvent e' b)) rsp

--------------------------------------------------------------------------------

data IEvent a = IEvent [Int]

data VEvent = forall a. VEvent (IEvent a) a

data RSPF next
  = Async (IO ()) next

  | Forever

  | forall a. Local (IEvent a -> RSP ()) next
  | forall a. EmitI (IEvent a) a next
  | forall a. AwaitI (IEvent a) (a -> next)

  | forall a. Await (Event a) (a -> next)
  | forall a. Or [RSP a] ((a, [RSP a]) -> next)
  | forall a. And [RSP a] ([a] -> next)

deriving instance Functor RSPF

newtype RSP a = RSP { getRSP :: Free RSPF a }
  deriving (Functor, Applicative, Monad)

async :: IO () -> RSP ()
async io = RSP $ liftF (Async io ())

local :: (IEvent a -> RSP ()) -> RSP ()
local f = RSP $ liftF (Local f ())

emitI :: IEvent a -> a -> RSP ()
emitI e a = RSP $ liftF (EmitI e a ())

awaitI :: IEvent a -> RSP a
awaitI e = RSP $ liftF (AwaitI e id)

await :: Event a -> RSP a
await e = RSP $ liftF (Await e id)

orr :: [RSP a] -> RSP (a, [RSP a])
orr rsps = RSP $ liftF (Or rsps id)

andd :: [RSP a] -> RSP [a]
andd rsps = RSP $ liftF (And rsps id)

forever :: RSP a
forever = RSP $ liftF Forever

--------------------------------------------------------------------------------

sameReference :: IORef a -> IORef b -> Bool
sameReference = unsafeCoerce ((==) :: IORef a -> IORef a -> Bool)

data Run a
  = Done a
  | Blocked (RSP a)
  | Next (RSP a)
  | forall b. Cont (IEvent b) b (RSP a -> RSP a)

isDone :: Run a -> Maybe a
isDone (Done a) = Just a
isDone _ = Nothing

isCont :: Run a -> Maybe (Run a)
isCont rsp@(Cont _ _ _) = Just rsp
isCont _ = Nothing

-- TODO: hack
{-# NOINLINE nextId #-}
nextId :: IORef Int
nextId = unsafePerformIO $ newIORef 0

runRSP :: Either VEvent (Event b) -> RSP a -> IO (Run a)
runRSP _ (RSP (Pure a)) = pure (Done a)
runRSP e (RSP (Free (Async io next))) = do
  forkIO io
  runRSP e (RSP next)

-- Await
runRSP (Right (Event currentEvent)) rsp@(RSP (Free (Await (Event event) next))) = do
  if sameReference currentEvent event
    then do
      a <- readIORef event
      case a of
        Just a' -> pure (Next $ RSP (next a'))
        Nothing -> pure (Blocked rsp)
    else
      pure (Blocked rsp)
runRSP _ rsp@(RSP (Free (Await _ _))) = pure (Blocked rsp)

-- Local
runRSP _ (RSP (Free (Local f next))) = do
  eid <- atomicModifyIORef' nextId $ \i -> (i + 1, i)
  pure $ Next $ do
    f (IEvent [eid])
    RSP next

-- IEmit
runRSP _ (RSP (Free (EmitI e a next))) = do
  traceIO "EMITI"
  pure $ Cont e a $ \_ -> RSP next

-- IAwait
runRSP (Left (VEvent (IEvent e) a)) rsp@(RSP (Free (AwaitI (IEvent event) next))) = do
  traceIO "AWAITI"
  if e == event
    then do
      traceIO "AWAITI SAME"
      pure (Next $ RSP (next $ unsafeCoerce a))
    else pure (Blocked rsp)
runRSP _ rsp@(RSP (Free (AwaitI _ _))) = pure (Blocked rsp)

-- Or
runRSP e (RSP (Free (Or rsps next))) = do
  as <- traverse (runRSP e) rsps

  case focus isDone as of
    Just (a, z) -> pure (Next $ RSP $ next (a, map collapse (without z)))
    Nothing -> case focus isCont as of
      Just (Cont e b f, z) -> pure $ Cont e b $ \rsp -> case rsp of
        RSP (Free (Or rsps' next')) -> RSP $ Free (Or (replace z (unsafeCoerce f) rsps') next')
        rsp' -> rsp'
      _ -> pure (Blocked $ RSP $ Free $ Or (map collapse as) next)

-- And
runRSP e (RSP (Free (And rsps next))) = do
  as <- traverse (runRSP e) rsps
  case traverse isDone as of
    Just as' -> pure (Next $ RSP $ next as')
    Nothing -> case focus isCont as of
      Just (Cont e b f, z) -> do
        traceIO "CONT"
        pure $ Cont e b $ \rsp -> case rsp of
          RSP (Free (And rsps' next')) -> RSP $ Free (And (replace z (unsafeCoerce f) rsps') next')
          rsp' -> error "Cont: And"
      _ -> if any isBlocked as
        then pure (Blocked $ RSP $ Free $ And (map collapse as) next)
        else pure (Next $ RSP $ Free $ And (map collapse as) next)
  where
    isBlocked (Done _) = False
    isBlocked (Blocked _) = True
    isBlocked (Next _) = False

collapse (Done a) = RSP (Pure a)
collapse (Blocked a) = a
collapse (Next b) = b

run :: RSP () -> IO Context
run rsp = Context <$> newMVar (Just rsp)

--------------------------------------------------------------------------------

data F f a = P a | F Int (f (F f a))

instance Functor f => Functor (F f) where
  fmap f = go where
    go (P a)  = P (f a)
    go (F x fa) = F x (go <$> fa)
  {-# INLINE fmap #-}

instance Functor f => Applicative (F f) where
  pure = P
  {-# INLINE pure #-}

  -- TODO
  f' <*> a' = do
    f <- f'
    a <- a'
    pure (f a)

instance Functor f => Monad (F f) where
  return = P
  {-# INLINE return #-}
  P a >>= f = f a
  F x m >>= f = F (x + 1) ((>>= f) <$> m)

--------------------------------------------------------------------------------

data Focus a = Focus a [a] [a]

focus :: (a -> Maybe b) -> [a] -> Maybe (b, Focus a)
focus f as = case break (isJust . f) as of
  (xs, y:ys) -> (,) <$> f y <*> Just (Focus y xs ys)
  (_, [])    -> Nothing

get :: Focus a -> a
get (Focus a _ _) = a

index :: Focus a -> Int
index = undefined

without :: Focus a -> [a]
without (Focus _ xs ys) = xs <> ys

replace :: Focus a -> (b -> b) -> [b] -> [b]
replace (Focus _ xs _) f bs
  = take (length xs) bs <> [f (head $ drop l bs)] <> drop (l + 1) bs
  where
    l = length xs

replace' :: Focus a -> b -> [b] -> [b]
replace' (Focus _ xs _) a bs
  = take (length xs) bs <> [a] <> drop (l + 1) bs
  where
    l = length xs

--------------------------------------------------------------------------------

data K a = forall v. K (IEvent v) v (RSP a) ([Int] -> RSP a -> R a)

data R a
  = D a
  | B (RSP a)
  | C (K a)
  -- | A (IO ()) (RSP a)

anyDone :: [R a] -> Either (a, [RSP a]) [RSP a]
anyDone = undefined

anyCont :: [R a] -> Maybe (K a, Focus (RSP a))
anyCont = undefined

allDone :: [R a] -> Either [a] [RSP a]
allDone = undefined

blocked :: [R a] -> Maybe [RSP a]
blocked = undefined

completeRSP :: [Int] -> Maybe (IEvent b, b) -> RSP a -> R a

completeRSP _ _ (RSP (Pure a)) = D a

completeRSP _ _ rsp@(RSP (Free Forever)) = B rsp

-- completeRSP _ _ (RSP (Free (Async io next))) = A io (RSP next)

completeRSP i@(n:ns) e (RSP (Free (Local f next))) =
  completeRSP (n + 1:ns) e (f (IEvent i) >> RSP next)

completeRSP (n:ns) (Just (IEvent e, v)) rsp@(RSP (Free (AwaitI (IEvent e') next))) =
  if e == e'
    then completeRSP (n + 1:ns) Nothing (RSP $ next $ unsafeCoerce v)
    else B rsp

completeRSP i@(n:ns) _ (RSP (Free (EmitI e v _))) =
  -- Set a temporary 'forever' placeholder so that the emit isn't reevaluated
  C $ K e v forever $ \i' rsp -> case rsp of
    RSP (Free (EmitI _ _ next))
      | i == i' -> completeRSP (n + 1:ns) Nothing (RSP next)
    _ -> B rsp

completeRSP i@(n:ns) e (RSP (Free (Or rsps next))) =
  case anyDone rs of
  -- 1. If a trail is done, next
      Left (a, rsps') -> completeRSP (n + 1:ns) Nothing (RSP $ next (a, rsps'))
      Right rsps'     -> case anyCont rs of
  -- 2. If a trail is a continuation, return a continuation
        Just (k, z) -> resume rsps' k z
  -- 3. If not a continuation, return blocked state
        Nothing     -> B $ RSP $ Free $ Or rsps' next

  where
    rs = map (\(m, rsp) -> completeRSP (0:m:n:ns) e rsp) (zip [0..] rsps)

    resume rsps' (K e v h k) z = C $ K e v (RSP $ Free $ Or (replace' z h rsps') next) $ \i' rsp -> case rsp of
       -- 2.1 Compare constructor and previous step id with current step id
       RSP (Free (Or rsps'' next'))
       -- 2.1.1 If there's match, continue with same id and no event
         | i == i' -> case k (0:index z:n:ns) (get z) of
             D a         -> completeRSP i' Nothing (RSP $ Free $ Or (replace' z (RSP $ Pure $ unsafeCoerce a) rsps'') next')
             B rsp       -> completeRSP i' Nothing (RSP $ Free $ Or (replace' z (unsafeCoerce rsp) rsps'') next')
             C k         -> resume rsps' k z
             -- A io next'' -> A io (RSP $ Free $ Or (replace' z (unsafeCoerce next'') rsps'') next')
       -- 2.1.2 Otherwise, return unchanged state
       _ -> B rsp

completeRSP (n:ns) e (RSP (Free (And rsps next))) =
  case allDone rs of
    Left as     -> completeRSP (n + 1:ns) Nothing (RSP $ next as)
    Right rsps' -> B $ RSP $ Free $ And rsps' next

  where
    rs = map (\(m, rsp) -> completeRSP (0:m:n:ns) e rsp) (zip [0..] rsps)

runR :: [[Int] -> RSP a -> R a] -> R a -> IO a
runR _ (D a) = pure a
runR [] (B rsp) = error "Program blocked"
runR (k:ks) (B rsp) = runR ks (k [0] rsp)
runR ks (C (K e v rsp k)) = runR (k:ks) rsp'
  where
    rsp' = completeRSP [0] (Just (e, v)) rsp

runProgram :: RSP a -> IO a
runProgram rsp = runR [] (completeRSP [0] Nothing rsp)

p1 = runProgram $ local $ \e -> do
  (a, _) <- orr [ Left <$> awaitI e, Right <$> emitI e "asd" ]
  pure $ trace ("A: " <> show a) ()

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

testRSP :: RSP ()
testRSP = undefined

type Application a = (a -> IO Context) -> IO Context

server :: Application (Event String)
server app = do
  e <- newEvent
  ctx <- app e
  emit ctx e "a"
  emit ctx e "b"
  pure ctx

m = server $ \x -> server $ \y -> server $ \z -> run $ do
  a <- andd [ await x, await y, await z ]
  async $ traceIO $ show a
  a <- fst <$> orr [ await x, await y, await z ]
  async $ traceIO a

m2 = server $ \x -> server $ \y -> server $ \z -> run $ do
  a <- andd [ await x, await x, await x ]
  async $ traceIO $ show a
  a <- andd [ await x, await x, await x ]
  async $ traceIO $ show a

boot :: Application (Event ())
boot app = do
  boot <- newEvent
  ctx <- app boot
  emit ctx boot ()
  pure ctx

m3 = boot $ \b -> run $ local $ \e -> do
  await b
  async $ traceIO "BOOTED"
  a <- andd [ Left <$> awaitI e, Right <$> emitI e "asd" ]
  async $ print a
