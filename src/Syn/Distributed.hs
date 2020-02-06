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

import Syn (Syn (..), SynF (..), V (..), Event, EventId, EventValue, NodeId (..))

--------------------------------------------------------------------------------

newtype Frame = Frame Int deriving (Eq, Ord, Num, Show)

type AwaitingId = Int
type EmittedId = Int

-- | An internal trail run
newtype Run = Run { getRun :: [(Frame, Set AwaitingId, Set EmittedId)] }

--------------------------------------------------------------------------------

transposeByFrame
  :: [(NodeId, [(Frame, Set AwaitingId, Set EmittedId)])]
  -> [(Frame, [(NodeId, Set AwaitingId, Set EmittedId)])]
transposeByFrame = undefined

data Coherent = Coherent | Restart deriving Show

cut :: [(NodeId, (Set AwaitingId, Set EmittedId))] -> [(NodeId, Coherent)]
cut [] = []
cut step = list $ extend f z
  where
    Just z = zipper step

    f :: ListZipper (NodeId, (Set AwaitingId, Set EmittedId)) -> (NodeId, Coherent)
    f (ListZipper ls (nid, (as, _)) rs) =
      ( nid
      , if S.size (S.intersection as otherEs) > 0
          then Restart
          else Coherent
      )
      where
        otherEs = S.unions [ es | (nid, (as, es)) <- ls <> rs ]

testCut = cut
  [ (0, (S.fromList [1, 2], S.fromList []))
  , (1, (S.fromList [3], S.fromList [1]))
  , (2, (S.fromList [], S.fromList [3]))
  ]

--------------------------------------------------------------------------------

type SynRun v a = [(Frame, Syn v a, Map EventId EventValue)]

bla :: [(Frame, v, Map EventId EventValue)] -> SynRun v a -> SynRun v a
bla upstream run = undefined
