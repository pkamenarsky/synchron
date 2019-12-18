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

data IEvent a = IEvent Int

data VEvent = forall a. VEvent (IEvent a) a

data RSPF next
  = Async (IO ()) next

  | forall a. Await (Event a) (a -> next)
  | forall a. Or [RSP a] ((a, [RSP a]) -> next)
  | forall a. And [RSP a] ([a] -> next)

deriving instance Functor RSPF

newtype RSP a = RSP { getRSP :: Free RSPF a }
  deriving (Functor, Applicative, Monad)

async :: IO () -> RSP ()
async io = RSP $ liftF (Async io ())

await :: Event a -> RSP a
await e = RSP $ liftF (Await e id)

orr :: [RSP a] -> RSP (a, [RSP a])
orr rsps = RSP $ liftF (Or rsps id)

andd :: [RSP a] -> RSP [a]
andd rsps = RSP $ liftF (And rsps id)

--------------------------------------------------------------------------------

sameReference :: IORef a -> IORef b -> Bool
sameReference = unsafeCoerce ((==) :: IORef a -> IORef a -> Bool)

data Run a
  = Done a
  | Blocked (RSP a)
  | Next (RSP a)
  | forall b. Cont (IEvent b) b (RSP a -> RSP a)

data Focus a = Focus a [a] [a]

focus :: (a -> Maybe b) -> [a] -> Maybe (b, Focus a)
focus f as = case break (isJust . f) as of
  (xs, y:ys) -> (,) <$> f y <*> Just (Focus y xs ys)
  (_, [])    -> Nothing

get :: Focus a -> a
get (Focus a _ _) = a

without :: Focus a -> [a]
without (Focus _ xs ys) = xs <> ys

replace :: Focus a -> (b -> b) -> [b] -> [b]
replace (Focus a xs _) f bs
  = take (length xs) bs <> [f (head $ drop l bs)] <> drop (l + 1) bs
  where
    l = length xs

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

many :: RSP (Either a [RSP ()]) -> RSP a
many k = do
  go [ Left <$> k ]
  where
    go ks = do
      a <- orr ks
      undefined

done :: a -> RSP (Either a [RSP ()])
done a = pure (Left a)

spawn :: RSP () -> RSP (Either a [RSP ()])
spawn k = pure (Right [k])

--------------------------------------------------------------------------------

data Δ a

local :: (Δ a -> RSP b) -> RSP b
local = undefined

with :: Δ a -> (a -> RSP (Either a b)) -> RSP b
with = undefined

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

-- m3 = boot $ \b -> run $ local $ \e -> do
--   await b
--   async $ traceIO "BOOTED"
--   a <- andd [ Left <$> awaitI e, Right <$> emitI e "asd" ]
--   async $ print a
