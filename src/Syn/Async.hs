{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Syn.Async where

import Control.Concurrent
import Control.Monad.Free

import Data.IORef
import Data.List
-- import Data.ListZipper
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import Syn (Syn (..), SynF (..), V (..), Event (..), EventId, EventValue (..), Trail (..), foldV)

type (~~>) = Map

(~~>) = M.singleton

--------------------------------------------------------------------------------

push :: Event t b -> b -> Trail v a -> IO ()
push = undefined

newTrail :: (([EventId], M.Map EventId EventValue) -> IO ()) -> Syn v a -> IO (Trail v a)
newTrail notify p' = undefined

-- advance :: Monoid v => Trail v a -> IO (Maybe a, V v, [EventId], M.Map EventId EventValue)
-- advance trail = modifyMVar (p trail) $ \p -> case p of
-- 
--   Syn (Free (Remote mkTrail next)) -> do
--     trail <- mkTrail
--     (a, v, e, m) <- advance trail
--     case a of
--       Just a' -> pure (Syn (next a'), (Nothing, v, e, m))
--       Nothing -> pure (Syn (Free (RemoteU trail next)), (Nothing, v, e, m))
-- 
--   Syn (Free (RemoteU trail next)) -> do
--     (a, v, e, m) <- advance trail
--     case a of
--       Just a' -> pure (Syn (next a'), (Nothing, v, e, m))
--       Nothing -> pure (Syn (Free (RemoteU trail next)), (Nothing, v, e, m))
-- 
--   Syn (Free (Await (Event eid) next)) -> do
--     pure (p, (Nothing, undefined, [eid], mempty))
-- 
--   Syn (Free (Emit e@(EventValue (Event eid) _) next)) -> do
--     pure (p, (Nothing, undefined, [], eid ~~> e))
-- 
--   Syn (Free (And p q next)) -> do
--     pt <- newTrail (notify trail) p
--     qt <- newTrail (notify trail) q
-- 
--     pr <- advance pt
--     qr <- advance qt
-- 
--     case (pr, qr) of
--       ((Just a, pv, pe, pm), (Just b, qv, qe, qm))
--         -> pure (Syn (next (a, b)), (Nothing, V (foldV pv <> foldV qv), pe <> qe, pm <> qm))
--       ((_, pv, pe, pm), (_, qv, qe, qm))
--         -> pure (Syn (Free (And_T pt qt next)), (Nothing, V (foldV pv <> foldV qv), pe <> qe, pm <> qm))

-- newTrail :: ((Maybe a, V v) -> IO ()) -> Syn v a -> IO (Trail v a)
-- newTrail notify rsp@(Syn (Free (And p q next))) = do
--   v  <- newMVar rsp
-- 
--   pt <- newTrail undefined p
--   qt <- newTrail undefined q
-- 
--   pure $ Trail
--     { advance = undefined
--         -- a <- advance pt m
--         -- b <- advance qt m
--         -- case (a, b) of
--         --   ((a', va, ra), (b', vb, rb)) -> undefined
--     }
-- newTrail notify (Syn (Free (Or f p q next))) = do
--   v <- newIORef p
--   pure $ Trail
--     { advance = undefined
--     }
