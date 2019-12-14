{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module RSP where

import Control.Concurrent
import Control.Monad.Free

import Data.IORef

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
      Just rsp -> (,True) <$> go e rsp
  where
    go e rsp = do
      a <- runRSP e rsp
      writeIORef event Nothing
      case a of
        Left _ -> pure Nothing
        (Right (Left rsp')) -> do
          pure (Just rsp')
        (Right (Right rsp')) -> do
          go e rsp'

--------------------------------------------------------------------------------

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

runRSP :: Event b -> RSP a -> IO (Either a (Either (RSP a) (RSP a)))
runRSP _ (RSP (Pure a)) = pure (Left a)
runRSP e (RSP (Free (Async io next))) = do
  forkIO io
  runRSP e (RSP next)
runRSP (Event currentEvent) rsp@(RSP (Free (Await (Event event) next))) = do
  if sameReference currentEvent event
    then do
      a <- readIORef event
      case a of
        Just a' -> pure (Right $ Right $ RSP (next a'))
        Nothing -> pure (Right $ Left rsp)
    else
      pure (Right $ Left rsp)
runRSP e (RSP (Free (Or rsps next))) = do
  as <- zip rsps <$> traverse (runRSP e) rsps
  go Nothing [] as
  where
    go (Just a) rs [] = pure (Right $ Right $ RSP $ next (a, rs))

    -- TODO: is this blocked? infinite loop?
    go Nothing rs []  = pure (Right $ Left $ RSP $ Free $ Or rs next)

    go (Just a) rs ((rsp, _):xs) = go (Just a) (rsp:rs) xs

    go Nothing rs (((_, Left a):xs)) = go (Just a) rs xs
    go Nothing rs (((_, Right (Left rsp')):xs)) = go Nothing (rsp':rs) xs
    go Nothing rs (((_, Right (Right rsp')):xs)) = go Nothing (rsp':rs) xs
runRSP e (RSP (Free (And rsps next))) = do
  as <- traverse (runRSP e) rsps
  case traverse done as of
    Just as' -> pure (Right $ Right $ RSP $ next as')
    Nothing -> if any isBlocked as
      then pure (Right $ Left $ RSP $ Free $ And (map collapse as) next)
      else pure (Right $ Right $ RSP $ Free $ And (map collapse as) next)
  where
    done (Left a) = Just a
    done _ = Nothing

    isBlocked (Left _) = False
    isBlocked (Right (Left _)) = True
    isBlocked (Right (Right _)) = False

    collapse (Left a) = RSP (Pure a)
    collapse (Right (Left a)) = a
    collapse (Right (Right b)) = b

run :: RSP () -> IO Context
run rsp = Context <$> newMVar (Just rsp)

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
