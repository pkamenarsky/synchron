{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module RSP where

import Control.Concurrent
import Control.Monad.Free

import Data.IORef

import Unsafe.Coerce (unsafeCoerce)

newtype Context = Context (MVar (Maybe (RSP ())))

newtype Event a = Event (IORef a)

newEvent :: IO (Event a)
newEvent = Event <$> newIORef undefined

emit :: Context -> Event a -> a -> IO Bool
emit (Context ctx) e@(Event event) a = do
  writeIORef event a
  modifyMVar ctx $ \rsp' -> case rsp' of
    Nothing -> pure (Nothing, False)
    Just rsp -> do
      rsp' <- runRSP e rsp
      case rsp' of
        Left _ -> pure (Nothing, True)
        Right rsp' -> pure (Just rsp', True)

--------------------------------------------------------------------------------

data RSPF next
  = forall a. Step (IO a) (a -> next)
  | forall a. Await (Event a) (a -> next)
  | forall a. Or [RSP a] ((a, [RSP a]) -> next)
  | forall a. And [RSP a] ([a] -> next)

deriving instance Functor RSPF

newtype RSP a = RSP { getRSP :: Free RSPF a }
  deriving (Functor, Applicative, Monad)

step :: IO a -> RSP a
step io = RSP $ liftF (Step io id)

await :: Event a -> RSP a
await e = RSP $ liftF (Await e id)

orr :: [RSP a] -> RSP (a, [RSP a])
orr rsps = RSP $ liftF (Or rsps id)

andd :: [RSP a] -> RSP [a]
andd rsps = RSP $ liftF (And rsps id)

--------------------------------------------------------------------------------

sameReference :: IORef a -> IORef b -> Bool
sameReference = unsafeCoerce ((==) :: IORef a -> IORef a -> Bool)

runRSP :: Event b -> RSP a -> IO (Either a (RSP a))
runRSP _ (RSP (Pure a)) = pure (Left a)
runRSP e (RSP (Free (Step io next))) = do
  a <- io
  runRSP e (RSP $ next a)
runRSP (Event currentEvent) rsp@(RSP (Free (Await (Event event) next))) = do
  if sameReference currentEvent event
    then do
      a <- readIORef event
      pure (Right $ RSP (next a))
    else
      pure (Right rsp)
runRSP e (RSP (Free (Or rsps next))) = do
  as <- zip rsps <$> traverse (runRSP e) rsps
  go Nothing [] as
  where
    go (Just a) rs [] = pure (Right $ RSP $ next (a, rs))
    go Nothing rs []  = pure (Right $ RSP $ Free $ Or rs next)

    go (Just a) rs ((rsp, _):xs) = go (Just a) (rsp:rs) xs

    go Nothing rs (((_, Left a):xs)) = go (Just a) rs xs
    go Nothing rs (((_, Right rsp'):xs)) = go Nothing (rsp':rs) xs
runRSP e (RSP (Free (And rsps next))) = do
  as <- traverse (runRSP e) rsps
  case (traverse done as, sequence as) of
    (Just as', _)    -> pure (Right $ RSP $ next as')
    (_, Right rsps') -> pure (Right $ RSP $ Free $ And rsps' next)
  where
    done (Left a) = Just a
    done _ = Nothing

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
  pure ctx

m = server $ \x -> server $ \y -> server $ \z -> run $ do
  a <- await x
  step $ print a
