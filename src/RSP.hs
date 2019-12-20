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

--------------------------------------------------------------------------------

data Event a = Event [Int]

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

data Run a
  = Done a
  | Blocked (RSP a)
  | Next (RSP a)
  | forall b. Cont (Event b) b (RSP a -> RSP a)

isDone :: Run a -> Maybe a
isDone (Done a) = Just a
isDone _ = Nothing

isCont :: Run a -> Maybe (Run a)
isCont rsp@(Cont _ _ _) = Just rsp
isCont _ = Nothing

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

data K a = forall v. K (Event v) v (RSP a -> R a)

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

-- completeRSP :: [Int] -> Maybe (Event b, b) -> RSP a -> R a
-- completeRSP _ _ (RSP (Pure a)) = D a
-- completeRSP i _ rsp@(RSP (Free Forever)) = B rsp
-- -- completeRSP _ _ (RSP (Free (Async io next))) = A io (RSP next)
-- completeRSP i@(n:ns) e (RSP (Free (Local f next))) =
--   completeRSP (n + 1:ns) e (f (Event i) >> RSP next)
-- completeRSP i@(n:ns) (Just (Event e, v)) rsp@(RSP (Free (Await (Event e') next))) =
--   if e == e'
--     then completeRSP (n + 1:ns) Nothing (RSP $ next $ unsafeCoerce v)
--     else B rsp
-- completeRSP i@(n:ns) _ (RSP (Free (Emit e v _))) =
--   -- Set a temporary 'forever' placeholder so that the emit isn't reevaluated
--   C $ K e v forever $ \i' rsp -> case rsp of
--     RSP (Free (Emit _ _ next))
--       | i == i' -> completeRSP (n + 1:ns) Nothing (RSP next)
--     _ -> B rsp
-- completeRSP i@(n:ns) e (RSP (Free (Or rsps next))) =
--   case anyDone rs of
--   -- 1. If a trail is done, next
--       Left (a, rsps') -> completeRSP (n + 1:ns) Nothing (RSP $ next (a, rsps'))
--       Right rsps'     -> case anyCont rs of
--   -- 2. If a trail is a continuation, return a continuation
--         Just (k, z) -> resume rsps' k z
--   -- 3. If not a continuation, return blocked state
--         Nothing     -> B $ RSP $ Free $ Or rsps' next
-- 
--   where
--     rs = map (\(m, rsp) -> completeRSP (0:m:n:ns) e rsp) (zip [0..] rsps)
-- 
--     resume rsps' (K e v h k) z = C $ K e v (RSP $ Free $ Or (replace' z h rsps') next) $ \i' rsp -> case rsp of
--        -- 2.1 Compare constructor and previous step id with current step id
--        RSP (Free (Or rsps'' next'))
--        -- 2.1.1 If there's match, continue with same id and no event
--          | i == i' -> case k (0:index z:n:ns) (get z) of
--              D a         -> completeRSP i' Nothing (RSP $ Free $ Or (replace' z (RSP $ Pure $ unsafeCoerce a) rsps'') next')
--              B rsp       -> completeRSP i' Nothing (RSP $ Free $ Or (replace' z (unsafeCoerce rsp) rsps'') next')
--              C k         -> resume rsps' k z
--              -- A io next'' -> A io (RSP $ Free $ Or (replace' z (unsafeCoerce next'') rsps'') next')
--        -- 2.1.2 Otherwise, return unchanged state
--        _ -> B rsp
-- completeRSP i@(n:ns) e (RSP (Free (And rsps next))) =
--   case allDone rs of
--     Left as     -> completeRSP (n + 1:ns) Nothing (RSP $ next as)
--     Right rsps' -> B $ RSP $ Free $ And rsps' next
-- 
--   where
--     rs = map (\(m, rsp) -> completeRSP (0:m:n:ns) e rsp) (zip [0..] rsps)

-- This doesn't change structure
reactRSP :: Event b -> b -> RSP a -> RSP a
reactRSP (Event e) a rsp@(RSP (Free (Await (Event e') next))) = if e == e'
  then RSP $ next $ unsafeCoerce a
  else rsp
reactRSP event@(Event e) a rsp@(RSP (Free (Or rsps next))) = RSP $ Free $ Or (map (reactRSP event a) rsps) next
reactRSP event@(Event e) a rsp@(RSP (Free (And rsps next))) = RSP $ Free $ And (map (reactRSP event a) rsps) next
reactRSP _ _ rsp = rsp

advanceRSP :: RSP a -> R a
advanceRSP (RSP (Pure a)) = D a
advanceRSP rsp@(RSP (Free Forever)) = B rsp
advanceRSP (RSP (Free (Local f next))) = advanceRSP (f (Event undefined) >> RSP next)
advanceRSP rsp@(RSP (Free (Await _ _))) = B rsp
advanceRSP (RSP (Free (Emit e v _))) = C $ K e v $ \rsp -> undefined
advanceRSP (RSP (Free (Or rsps next))) =
  case anyDone rs of
      Left (a, rsps') -> advanceRSP (RSP $ next (a, rsps'))
      Right rsps'     -> case anyCont rs of
        Just (k, z) -> resume rsps' k z
        Nothing     -> B $ RSP $ Free $ Or rsps' next

  where
    rs = map advanceRSP rsps

    resume rsps' (K e v k) z = C $ K e v $ \rsp -> case rsp of
       RSP (Free (Or rsps'' next')) -> case k (get z) of
             D a         -> advanceRSP (RSP $ Free $ Or (replace' z (RSP $ Pure $ unsafeCoerce a) rsps'') next')
             B rsp       -> advanceRSP (RSP $ Free $ Or (replace' z (unsafeCoerce rsp) rsps'') next')
             C k         -> resume rsps' k z
       _ -> B rsp

advanceRSP (RSP (Free (And rsps next))) =
  case allDone rs of
    Left as     -> advanceRSP (RSP $ next as)
    Right rsps' -> B $ RSP $ Free $ And rsps' next

  where
    rs = map (\(m, rsp) -> advanceRSP rsp) (zip [0..] rsps)

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
