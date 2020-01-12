{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Syn.Distributed where

import Control.Comonad
import Control.Monad.Free

import Data.IORef
import Data.List
import Data.ListZipper
import           Data.Map (Map)
import           Data.Set (Set)
import qualified Data.Set as S

import Syn (Syn (..), SynF (..), V (..), Event, EventId, EventValue)

--------------------------------------------------------------------------------

newtype AwaitingId = AwaitingId EventId deriving (Eq, Ord, Show)
newtype EmittedId = EmittedId EventId deriving (Eq, Ord, Show)

-- | An internal trail run
newtype Run = Run { getRun :: [(Set AwaitingId, Set EmittedId)] }

instance Semigroup Run where
  Run p <> Run q = Run $ map (\((pa, pe), (qa, qe)) -> (pa <> qa, pe <> qe)) (zip p q)

--------------------------------------------------------------------------------

newtype Frame = Frame Int deriving (Eq, Ord, Num, Show)

data Trail v a = Trail
  { advance :: [Map EventId EventValue] -> IO ()
  , commit  :: Frame -> IO ()
  }

push :: Event t b -> b -> Trail v a -> IO ()
push = undefined

newTrail :: ((Maybe a, V v, Run) -> IO ()) -> Syn v a -> IO (Trail v a)
newTrail notify (Syn (Free (And p q next))) = do
  pt <- newTrail undefined p
  qt <- newTrail undefined q

  pure $ Trail
    { advance = \run -> undefined
        -- a <- advance pt m
        -- b <- advance qt m
        -- case (a, b) of
        --   ((a', va, ra), (b', vb, rb)) -> undefined

    , commit  = undefined
    }
newTrail notify (Syn (Free (Or p q next))) = do
  v <- newIORef p
  pure $ Trail
    { advance = undefined
    , commit  = undefined
    }

--------------------------------------------------------------------------------

data Coherent
  = Coherent                -- ^ Not affected by other trails
  | Restart [EmittedId]     -- ^ Affected by Coherent trails
  | Unknown                 -- ^ Affected by Restart trails

cut :: [Maybe (Set AwaitingId, Set EmittedId)] -> [Coherent]
cut step = list $ extend undefined z
  where
    Just z = zipper step

coherencyCut :: [Run] -> [[Coherent]]
coherencyCut trails = undefined
  where
    maxLength = maximum (map (length . getRun) trails)
    padToMax xs = map Just xs <> replicate (maxLength - length xs) Nothing

    frames =
      [ undefined
      | steps <- transpose (map (padToMax . getRun) trails)
      ]
