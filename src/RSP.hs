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

import Unsafe.Coerce (unsafeCoerce)

import Debug.Trace

--------------------------------------------------------------------------------

type EventId = IORef ()

data Event a = Event EventId

data RSPF next
  = Async (IO ()) next

  | Forever

  | forall a b. Local (Event a -> RSP b) (b -> next)
  | forall a. Emit (Event a) a next
  | forall a. Await (Event a) (a -> next)

  | forall a. Or [RSP a] ((a, [RSP a]) -> next)
  | forall a. And [RSP a] ([a] -> next)

deriving instance Functor RSPF

newtype RSP a = RSP { getRSP :: Free RSPF a }
  deriving (Functor, Applicative, Monad)

async :: IO () -> RSP ()
async io = RSP $ liftF (Async io ())

local :: (Event a -> RSP b) -> RSP b
local f = RSP $ liftF (Local f id)

emit :: Event a -> a -> RSP ()
emit e a = RSP $ liftF (Emit e a ())

await :: Event a -> RSP a
await e = RSP $ liftF (Await e id)

orr' :: [RSP a] -> RSP (a, [RSP a])
orr' rsps = RSP $ liftF (Or rsps id)

orr :: [RSP a] -> RSP a
orr rsps = fmap fst $ RSP $ liftF (Or rsps id)

andd :: [RSP a] -> RSP [a]
andd rsps = RSP $ liftF (And rsps id)

forever :: RSP a
forever = RSP $ liftF Forever

--------------------------------------------------------------------------------

data Focus a = Focus a [a] [a]
  deriving (Show, Functor)

focus :: (a -> Either b c) -> [a] -> Either (b, Focus a) [c]
focus f as = go [] as []
  where
    go _ [] rs = Right rs
    go ls (a:as) rs = case f a of
      Left b  -> Left (b, Focus a ls as)
      Right c -> go (ls <> [a]) as (rs <> [c])

get :: Focus a -> a
get (Focus a _ _) = a

without :: Focus a -> [a]
without (Focus _ xs ys) = xs <> ys

modify :: Focus a -> (b -> b) -> [b] -> [b]
modify (Focus _ xs _) f bs
  = take (length xs) bs <> [f (head $ drop l bs)] <> drop (l + 1) bs
  where
    l = length xs

replace :: Focus a -> b -> [b] -> [b]
replace (Focus _ xs _) a bs
  = take (length xs) bs <> [a] <> drop (l + 1) bs
  where
    l = length xs

--------------------------------------------------------------------------------

data K a = forall v. K (Event v) v (RSP a -> IO (R a))

data R a
  = D a              -- Done
  | B (RSP a)        -- Blocked
  | C (RSP a) (K a)  -- Continuation

isDone :: R a -> Either a (R a)
isDone (D a) = Left a
isDone r     = Right r

anyDone :: [(R a, RSP a)] -> Either (a, Focus (RSP a)) [R a]
anyDone rs = case focus (isDone . fst) rs of
  Left (a, z) -> Left (a, snd <$> z)
  Right rbcs  -> Right rbcs

allDone :: [R a] -> Either [a] [R a]
allDone rs = case traverse (swap . isDone) rs of
  Right as -> Left as
  Left _   -> Right rs
  where
    swap (Left a)  = Right a
    swap (Right b) = Left b

anyCont :: [(R a, RSP a)] -> Either (K a, Focus (RSP a)) [RSP a]
anyCont rs = case focus isCont rs of
  Left (k, z) -> Left (k, snd <$> z)
  Right rsps  -> Right rsps
  where
    isCont (C _ k, _) = Left k
    isCont (B rsp, _) = Right rsp
    isCont (D a, _)   = Right (RSP $ Pure a)

-- This doesn't change structure
reactRSP :: Event b -> b -> RSP a -> RSP a
-- Await
reactRSP (Event e) a rsp@(RSP (Free (Await (Event e') next))) = if e == e'
  then RSP $ next $ unsafeCoerce a
  else rsp
-- Or
reactRSP event@(Event e) a rsp@(RSP (Free (Or rsps next)))
  = RSP $ Free $ Or (map (reactRSP event a) rsps) next
-- And
reactRSP event@(Event e) a rsp@(RSP (Free (And rsps next)))
  = RSP $ Free $ And (map (reactRSP event a) rsps) next
-- _
reactRSP _ _ rsp = rsp

advanceRSP :: RSP a -> IO (R a)
-- Pure
advanceRSP (RSP (Pure a)) = pure (D a)
-- Forever
advanceRSP rsp@(RSP (Free Forever)) = pure (B rsp)
-- Async
advanceRSP rsp@(RSP (Free (Async io next))) = do
  forkIO io
  advanceRSP (RSP next)
-- Local
advanceRSP (RSP (Free (Local f next))) = do
  eid <- newIORef ()
  advanceRSP (f (Event eid) >>= RSP . next)
-- Await
advanceRSP rsp@(RSP (Free (Await _ _))) = pure (B rsp)
-- Emit
advanceRSP rsp@(RSP (Free (Emit e v next)))
  = pure (C rsp $ K e v $ \_ -> advanceRSP (RSP next))
-- Or
advanceRSP rsp@(RSP (Free (Or rsps next))) = do
  as <- traverse advanceRSP rsps

  case anyDone (zip as rsps) of
      Left (a, z) -> advanceRSP (RSP $ next (a, without z))
      Right rbcs  -> case anyCont (zip rbcs rsps) of
        Left (k, z) -> pure (resume rsp k z)
        Right rsps' -> pure (B $ RSP $ Free $ Or rsps' next)
-- And
advanceRSP rsp@(RSP (Free (And rsps next))) = do
  as <- traverse advanceRSP rsps

  case allDone as of
    Left as    -> advanceRSP (RSP $ next as)
    Right rbcs -> case anyCont (zip rbcs rsps) of
      Left (k, z) -> pure (resume rsp k z)
      Right rsps' -> pure (B $ RSP $ Free $ And rsps' next)

advanceFocused :: Focus (RSP a) -> RSP a -> RSP b -> RSP b
advanceFocused z rsp (RSP (Free (Or rsps next)))  = RSP $ Free $ Or  (replace z (unsafeCoerce rsp) rsps) next
advanceFocused z rsp (RSP (Free (And rsps next))) = RSP $ Free $ And (replace z (unsafeCoerce rsp) rsps) next
advanceFocused _ _ _ = error "advanceFocused"

resume :: RSP b -> K a -> Focus (RSP a) -> R b
resume hole (K e v k) z = C hole $ K e v $ \rsp -> do
  a <- k (get z)

  case a of
    D a    -> advanceRSP (advanceFocused z (RSP $ Pure a) rsp)
    B rsp' -> advanceRSP (advanceFocused z rsp' rsp)
    C _ k  -> pure (resume hole k z)

--------------------------------------------------------------------------------

data Result a = Done a | ProgramBlocked | StackNotEmpty
  deriving Show

runRSP' :: [RSP a -> IO (R a)] -> Maybe (Event b, b) -> RSP a -> IO (Result a)
runRSP' ks (Just (e, a)) rsp = traceIO "react" >> runRSP' ks Nothing (reactRSP e a rsp)
runRSP' [] Nothing rsp       = traceIO "advance" >> (advanceRSP rsp >>= runR [])
runRSP' (k:ks) Nothing rsp   = traceIO "resume" >> (k rsp >>= runR ks)

runR :: [RSP a -> IO (R a)] -> R a -> IO (Result a)
runR []     (D a)          = pure (Done a)
runR (k:ks) (D a)          = pure StackNotEmpty
runR []     (B _)          = pure ProgramBlocked
runR ks     (B rsp')       = traceIO "blocked" >> runRSP' ks Nothing rsp'
runR ks (C rsp' (K e v k)) = do
  traceIO ("L: " <> show (length ks))
  traceIO "continue" >> runRSP' (k:ks) (Just (e, v)) rsp'

runRSP :: RSP a -> IO (Result a)
runRSP = runRSP' [] Nothing

--------------------------------------------------------------------------------

p1 = runRSP $ local $ \e -> do
  a <- andd [ Left <$> ((,) <$> await e <*> await e), Right <$> emit e "A", Right <$> emit e "C" ]
  async $ traceIO (show a)
  a <- orr [ Left <$> await e, Right <$> emit e "B" ]
  pure a

p2 = runRSP $ local $ \e -> do
  a <- andd [ Left <$> emit e "E", Right <$> await e ]
  pure a

p3 = runRSP $ local $ \e -> local $ \f -> do
  a <- andd
    [ Left  <$> (await e >> emit f "F")
    , Right <$> await f
    , Left  <$> emit e "E"
    ]
  pure a

p4 = runRSP $ local $ \e -> local $ \f -> do
  a <- andd
    [ Left  <$> andd [ Left <$> await e, Right <$> emit f "F" ]
    , Right <$> await f
    , Left  <$> andd [ Left <$> pure "_", Right <$> (await f >> emit e "E") ]
    ]
  pure a

p5 = runRSP $ local $ \e -> do
  andd
    [ Left  <$> go 0 e
    , Right <$> do
        emit e (Left 4)
        async $ traceIO "0"
        emit e (Left 4)
        async $ traceIO "1"
        emit e (Left 4)
        async $ traceIO "2"
        emit e (Left 4)
        emit e (Right ())
    ]
  where
    go :: Int -> Event (Either Int ()) -> RSP Int
    go s e = do
      a <- await e
      case a of
        Left n  -> go (s + n) e
        Right _ -> pure s

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

-- testRSP :: RSP ()
-- testRSP = undefined
-- 
-- type Application a = (a -> IO Context) -> IO Context
-- 
-- server :: Application (Event String)
-- server app = do
--   e <- newEvent
--   ctx <- app e
--   emit ctx e "a"
--   emit ctx e "b"
--   pure ctx
-- 
-- m = server $ \x -> server $ \y -> server $ \z -> run $ do
--   a <- andd [ await x, await y, await z ]
--   async $ traceIO $ show a
--   a <- fst <$> orr [ await x, await y, await z ]
--   async $ traceIO a
-- 
-- m2 = server $ \x -> server $ \y -> server $ \z -> run $ do
--   a <- andd [ await x, await x, await x ]
--   async $ traceIO $ show a
--   a <- andd [ await x, await x, await x ]
--   async $ traceIO $ show a
-- 
-- boot :: Application (Event ())
-- boot app = do
--   boot <- newEvent
--   ctx <- app boot
--   emit ctx boot ()
--   pure ctx
-- 
-- m3 = boot $ \b -> run $ local $ \e -> do
--   await b
--   async $ traceIO "BOOTED"
--   a <- andd [ Left <$> awaitI e, Right <$> emitI e "asd" ]
--   async $ print a
