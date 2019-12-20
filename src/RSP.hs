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
import Data.Either (isLeft)

import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Debug.Trace

--------------------------------------------------------------------------------

type EventId = Int

data Event a = Event EventId

data RSPF next
  = Async (IO ()) next

  | Forever

  | forall a. Local (Event a -> RSP ()) next
  | forall a. Emit (Event a) a next
  | forall a. Await (Event a) (a -> next)

  | forall a. Or [RSP a] ((a, [RSP a]) -> next)
  | forall a. And [RSP a] ([a] -> next)

deriving instance Functor RSPF

newtype RSP a = RSP { getRSP :: Free RSPF a }
  deriving (Functor, Applicative, Monad)

async :: IO () -> RSP ()
async io = RSP $ liftF (Async io ())

local :: (Event a -> RSP ()) -> RSP ()
local f = RSP $ liftF (Local f ())

emit :: Event a -> a -> RSP ()
emit e a = RSP $ liftF (Emit e a ())

await :: Event a -> RSP a
await e = RSP $ liftF (Await e id)

orr :: [RSP a] -> RSP (a, [RSP a])
orr rsps = RSP $ liftF (Or rsps id)

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

data K a = forall v. K (Event v) v (RSP a -> R a)

data R a
  = D a
  | B (RSP a)
  | C (K a)

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
    isCont (C k, _)   = Left k
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

advanceRSP :: RSP a -> R a
-- Pure
advanceRSP (RSP (Pure a))                = D a
-- Forever
advanceRSP rsp@(RSP (Free Forever))      = B rsp
-- Local
advanceRSP (RSP (Free (Local f next)))   = advanceRSP (f (Event undefined) >> RSP next)
-- Await
advanceRSP rsp@(RSP (Free (Await _ _)))  = B rsp
-- Emit
advanceRSP (RSP (Free (Emit e v next)))  = C $ K e v $ \_ -> advanceRSP (RSP next)
-- Or
advanceRSP (RSP (Free (Or rsps next))) =
  case anyDone (zip (map advanceRSP rsps) rsps) of
      Left (a, z) -> advanceRSP (RSP $ next (a, without z))
      Right rbcs  -> case anyCont (zip rbcs rsps) of
        Left (k, z) -> resume rsps k z
        Right rsps' -> B $ RSP $ Free $ Or rsps' next
-- And
advanceRSP (RSP (Free (And rsps next))) =
  case allDone (map advanceRSP rsps) of
    Left as    -> advanceRSP (RSP $ next as)
    Right rbcs -> case anyCont (zip rbcs rsps) of
      Left (k, z) -> resume rsps k z
      Right rsps' -> B $ RSP $ Free $ And rsps' next

resume :: [RSP a] -> K a -> Focus (RSP a) -> R b
resume rsps (K e v k) z = C $ K e v $ \rsp -> case rsp of
   RSP (Free (Or rsps' next')) -> case k (get z) of
         D a         -> advanceRSP (RSP $ Free $ Or (replace z (RSP $ Pure $ unsafeCoerce a) rsps') next')
         B rsp       -> advanceRSP (RSP $ Free $ Or (replace z (unsafeCoerce rsp) rsps') next')
         C k         -> resume rsps k z
   _ -> error "advanceRSP: Or"

-- runR :: [Int] -> [([Int], [Int] -> RSP a -> R a)] -> R a -> IO a
-- runR _ _ (D a) = pure a
-- runR _ [] (B _ rsp) = error "Program blocked"
-- runR _ ((i, k):ks) (B i' rsp) = runR i' ks (k i rsp)
-- runR i ks (C (K e v rsp k)) = runR (error "runR") ((i, k):ks) rsp'
--   where
--     rsp' = completeRSP i (Just (e, v)) rsp
-- 
-- runProgram :: RSP a -> IO a
-- runProgram rsp = case completeRSP [0] Nothing rsp of
--   b@(B i next) -> runR i [] b
-- 
-- p1 = runProgram $ local $ \e -> do
--   (a, _) <- orr [ Left <$> await e, Right <$> emit e "asd" ]
--   pure $ trace ("A: " <> show a) ()

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
