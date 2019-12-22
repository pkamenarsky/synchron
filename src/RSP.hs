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
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.State as ST
import qualified Control.Lens as L

import Data.IORef
import Data.List (intercalate)
import Data.Maybe (listToMaybe, mapMaybe)

import Unsafe.Coerce (unsafeCoerce)

import Debug.Trace

--------------------------------------------------------------------------------

type EventId = IORef ()

data Event a = Event EventId

type IsHole = Bool

data RSPF next
  = Async (IO ()) next

  | Forever

  | forall a b. Local (Event a -> RSP b) (b -> next)
  | forall a. Emit (Event a) a next
  | forall a. Await (Event a) (a -> next)

  | forall a. Or IsHole [RSP a] ((a, [RSP a]) -> next)
  | forall a. And IsHole [RSP a] ([a] -> next)

deriving instance Functor RSPF

newtype RSP a = RSP { getRSP :: Free RSPF a }
  deriving (Functor, Applicative, Monad)

instance Show (RSP a) where
  show (RSP (Pure a)) = "Pure"
  show (RSP (Free (Async _ _))) = "Async"
  show (RSP (Free Forever)) = "Forever"
  show (RSP (Free (Local _ _))) = "Local"
  show (RSP (Free (Emit _ _ _))) = "Emit"
  show (RSP (Free (Await _ _))) = "Await"
  show (RSP (Free (Or _ rsps _))) = "Or [" <> intercalate " " (map show rsps) <> "]"
  show (RSP (Free (And _ rsps _))) = "And [" <> intercalate " " (map show rsps) <> "]"

async :: IO () -> RSP ()
async io = RSP $ liftF (Async io ())

local :: (Event a -> RSP b) -> RSP b
local f = RSP $ liftF (Local f id)

emit :: Event a -> a -> RSP ()
emit e a = RSP $ liftF (Emit e a ())

await :: Event a -> RSP a
await e = RSP $ liftF (Await e id)

orr' :: [RSP a] -> RSP (a, [RSP a])
orr' rsps = RSP $ liftF (Or False rsps id)

orr :: [RSP a] -> RSP a
orr rsps = fmap fst $ RSP $ liftF (Or False rsps id)

andd :: [RSP a] -> RSP [a]
andd rsps = RSP $ liftF (And False rsps id)

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
reactRSP event@(Event e) a rsp@(RSP (Free (Or h rsps next)))
  = RSP $ Free $ Or h (map (reactRSP event a) rsps) next
-- And
reactRSP event@(Event e) a rsp@(RSP (Free (And h rsps next)))
  = RSP $ Free $ And h (map (reactRSP event a) rsps) next
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
  = pure (C forever $ K e v $ \_ -> advanceRSP (RSP next))
-- Or
advanceRSP rsp@(RSP (Free (Or h rsps next))) = do
  as <- traverse advanceRSP rsps

  case anyDone (zip as rsps) of
    Left (a, z) -> advanceRSP (RSP $ next (a, without z))
    Right rbcs  -> case anyCont (zip rbcs rsps) of
      Left (k, z) -> pure (resume (RSP (Free (Or True rsps next))) k z)
      Right rsps' -> pure (B $ RSP $ Free $ Or undefined rsps' next)
-- And
advanceRSP rsp@(RSP (Free (And h rsps next))) = do
  as <- traverse advanceRSP rsps

  case allDone as of
    Left as    -> advanceRSP (RSP $ next as)
    Right rbcs -> case anyCont (zip rbcs rsps) of
      Left (k, z) -> pure (resume (RSP (Free (And True rsps next))) k z)
      Right rsps' -> pure (B $ RSP $ Free $ And undefined rsps' next)

advanceFocused :: Focus (RSP a) -> RSP a -> RSP b -> RSP b
advanceFocused z rsp (RSP (Free (Or h rsps next)))  = RSP $ Free $ Or  h (replace z (unsafeCoerce rsp) rsps) next
advanceFocused z rsp (RSP (Free (And h rsps next))) = RSP $ Free $ And h (replace z (unsafeCoerce rsp) rsps) next
advanceFocused _ _ _ = error "advanceFocused"

resume :: RSP b -> K a -> Focus (RSP a) -> R b
resume hole (K e v k) z = C hole $ K e v $ \rsp -> do
  a <- k (get z)

  case a of
    D a    -> advanceRSP (advanceFocused z (RSP $ Pure a) rsp)
    B rsp' -> advanceRSP (advanceFocused z rsp' rsp)
    C _ k  -> pure (resume rsp k z)

--------------------------------------------------------------------------------

rspsL :: L.Lens' (RSP a) [RSP b]
rspsL = L.lens get set
  where
    get (RSP (Free (Or _ rsps _))) = unsafeCoerce rsps
    get (RSP (Free (And _ rsps _))) = unsafeCoerce rsps

    set (RSP (Free (Or h _ next))) rsps = RSP (Free (Or h (unsafeCoerce rsps) next))
    set (RSP (Free (And h _ next))) rsps = RSP (Free (And h (unsafeCoerce rsps) next))

ix :: Int -> L.Lens' [a] a
ix i = L.lens get set
  where
    get ls = ls !! i
    set ls a = take i ls <> [a] <> drop (i + 1) ls

-- This doesn't change structure
reactRSP' :: Maybe (Event b, b) -> RSP a -> IO (RSP a)
reactRSP' event rsp@(RSP (Free (Async io next))) = do
  forkIO io
  reactRSP' event (RSP next)
reactRSP' event rsp@(RSP (Free (Local f next))) = do
  eid <- liftIO $ newIORef ()
  reactRSP' event (f (Event eid) >>= RSP . next)
reactRSP' (Just (Event e, a)) rsp@(RSP (Free (Await (Event e') next))) = if e == e'
  then reactRSP' Nothing $ RSP $ next $ unsafeCoerce a
  else pure rsp
reactRSP' event rsp@(RSP (Free (Or h rsps next))) = do
  rsps' <- traverse (reactRSP' event) rsps
  pure $ RSP $ Free $ Or h rsps' next
reactRSP' event rsp@(RSP (Free (And h rsps next))) = do
  rsps' <- traverse (reactRSP' event) rsps
  pure $ RSP $ Free $ And h rsps' next
reactRSP' _ rsp = pure rsp

--------------------------------------------------------------------------------

modifyM :: Monad m => (st -> ST.StateT st m b) -> ST.StateT st m b
modifyM f = ST.get >>= f

advanceST :: L.Lens' (RSP a) (RSP b) -> Maybe (Event c, c) -> RSP b -> ST.StateT (RSP a) IO ()
advanceST l _ rsp@(RSP (Pure a)) = L.zoom l (ST.put rsp)
advanceST l _ rsp@(RSP (Free Forever)) = L.zoom l (ST.put rsp)
advanceST l (Just (Event e, a)) rsp@(RSP (Free (Await (Event e') next))) = do
  if e == e'
    then advanceST l Nothing (RSP $ next $ unsafeCoerce a)
    else L.zoom l (ST.put rsp)
advanceST l Nothing rsp@(RSP (Free (Await _ _))) = L.zoom l (ST.put rsp)
advanceST l event rsp@(RSP (Free (Local f next))) = do
  eid <- liftIO $ newIORef ()
  advanceST l event (f (Event eid) >>= RSP . next)
advanceST l event rsp@(RSP (Free (Async io next))) = do
  liftIO $ forkIO io
  advanceST l event (RSP next)
advanceST l _ rsp@(RSP (Free (Emit event a next))) = do
  L.zoom l (ST.put forever)
  ST.get >>= advanceST id (Just (event, a))
  advanceST l Nothing (RSP next)
advanceST l event rsp@(RSP (Free (And h rsps _))) = do
  sequence_
    [ do
        st <- ST.get
        advanceST (l . rspsL . ix i) event (st L.^. (l . rspsL . ix i))
    | (i, _) <- zip [0..] rsps
    ]

  st <- L.zoom l $ ST.get

  case st of
    rsp'@(RSP (Free (And h rsps next))) -> do
      liftIO $ traceIO (show rsp')
  
      case traverse rspDone rsps of
        Just as -> advanceST l Nothing (RSP (next $ unsafeCoerce as))
        Nothing -> pure ()
    rsp' -> advanceST l Nothing rsp'

advanceST l event rsp@(RSP (Free (Or h rsps _))) = do
  sequence_
    [ do
        st <- ST.get
        advanceST (l . rspsL . ix i) event (st L.^. (l . rspsL . ix i))
    | (i, _) <- zip [0..] rsps
    ]

  st <- L.zoom l $ ST.get

  case st of
    rsp'@(RSP (Free (Or h rsps next))) -> do
      liftIO $ traceIO (show rsp')
      
      case firstJust rspDone rsps of
        Just a  -> advanceST l Nothing (RSP (next (a, undefined)))
        Nothing -> pure ()
    rsp' -> advanceST l Nothing rsp'
    
rspDone (RSP (Pure a)) = Just a
rspDone _ = Nothing

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

runST :: RSP a -> IO (Result a)
runST rsp = do
  (_, st) <- ST.runStateT (advanceST id Nothing rsp) rsp
  case st of
    RSP (Pure a) -> pure (Done a)
    _            -> pure ProgramBlocked

--------------------------------------------------------------------------------

data Result a = Done a | ProgramBlocked | StackNotEmpty
  deriving Show

runRSP' :: [RSP a -> IO (R a)] -> Maybe (Event b, b) -> RSP a -> IO (Result a)
runRSP' ks (Just (e, a)) rsp = runRSP' ks Nothing rsp
runRSP' [] Nothing rsp       = advanceRSP rsp >>= runR []
runRSP' (k:ks) Nothing rsp   = k rsp >>= runR ks

runR :: [RSP a -> IO (R a)] -> R a -> IO (Result a)
runR []     (D a)         = pure (Done a)
runR (k:ks) (D a)         = pure StackNotEmpty
runR []     (B _)         = pure ProgramBlocked
runR ks     (B rsp)       = runRSP' ks Nothing rsp
runR ks (C rsp (K e v k)) = runRSP' (k:ks) (Just (e, v)) rsp

runRSP :: RSP a -> IO (Result a)
runRSP = runRSP' [] Nothing

--------------------------------------------------------------------------------

p1 = runST $ local $ \e -> do
  a <- andd [ Left <$> ((,) <$> await e <*> await e), Right <$> emit e "A", Right <$> emit e "C" ]
  async $ traceIO (show a)
  a <- orr [ Left <$> await e, Right <$> emit e "B" ]
  pure a

p2 = runST $ local $ \e -> do
  a <- andd [ Left <$> emit e "E", Right <$> await e ]
  pure a

p3 = runST $ local $ \e -> local $ \f -> do
  a <- andd
    [ Left  <$> (await e >> emit f "F")
    , Right <$> await f
    , Left  <$> emit e "E"
    ]
  pure a

p4 = runST $ local $ \e -> local $ \f -> do
  a <- andd
    [ Left  <$> andd [ Left <$> await e, Right <$> emit f "F" ]
    , Right <$> await f
    , Left  <$> andd [ Left <$> pure "_", Right <$> (await f >> emit e "E") ]
    ]
  pure a

p5 = runST $ local $ \e -> do
  andd
    [ Left  <$> go 0 e
    , Right <$> do
        emit e (Left 1)
        emit e (Left 2)
        emit e (Left 3)
        emit e (Left 4)
        emit e (Right ())
    ]
  where
    go :: Int -> Event (Either Int ()) -> RSP Int
    go s e = do
      a <- await e
      async $ traceIO "BLA"
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
