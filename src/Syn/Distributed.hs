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

newtype AwaitingId = AwaitingId EventId deriving (Eq, Ord, Show)
newtype EmittedId = EmittedId EventId deriving (Eq, Ord, Show)

-- | An internal trail run
newtype Run = Run { getRun :: [(Frame, Set AwaitingId, Set EmittedId)] }

--------------------------------------------------------------------------------

transposeByFrame
  :: [(NodeId, [(Frame, Set AwaitingId, Set EmittedId)])]
  -> [(Frame, [(NodeId, Set AwaitingId, Set EmittedId)])]
transposeByFrame = undefined

data Coherent = Coherent | Restart

coherencyCut
  :: [(NodeId, Set AwaitingId, Set EmittedId)]
  -> [(NodeId, Coherent)]
coherencyCut = undefined
