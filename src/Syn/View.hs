{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings           #-}

module Syn.View where

import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Reader as R

import Data.Bifunctor (second)

import qualified Data.Text                as T

import qualified Replica.VDOM             as R

import qualified Syn as Syn

type Path = [Int]

type ViewPatch v = (Path, v)

newtype VSyn v a = VSyn (R.ReaderT (Path, Syn.Event Syn.Internal [ViewPatch v]) (Syn.Syn ()) a)
  deriving (Functor, Applicative, Monad)

liftSyn :: Syn.Syn () a -> VSyn v a
liftSyn  = VSyn . lift

mapView :: (u -> v) -> VSyn u a -> VSyn v a
mapView f (VSyn p) = VSyn $ R.ReaderT $ \(path, e') -> Syn.local' (<>) $ \e -> do
  go path e e'
  where
    go path e e' = do
      (r, _, ks) <- Syn.unsafeRunOrr $ mconcat
        [ Left  <$> Syn.liftOrr (R.runReaderT p (path, e))
        , Right <$> Syn.liftOrr (Syn.await e)
        ]
      case r of
        Left a  -> pure a
        Right v -> do
          Syn.emit e' (fmap (second f) v)
          go path e e'

await :: Syn.Event t a -> VSyn v a
await = liftSyn . Syn.await

emit :: Syn.Event Syn.Internal a -> a -> VSyn v ()
emit e a = liftSyn (Syn.emit e a)

local :: VSyn v (Syn.Event Syn.Internal a)
local = liftSyn $ Syn.local pure

view :: v -> [VSyn v a] -> VSyn v a
view v children = VSyn $ R.ReaderT $ \(path, e) -> do
  Syn.orr $ mconcat
    [ [ Syn.emit e [(path, v)] >> Syn.forever ]
    , [ R.runReaderT child (index:path, e)
      | (VSyn child, index) <- zip children [0..]
      ]
    ]

text :: T.Text -> VSyn R.HTML a
text txt = view [R.VText txt] []

div_ :: [VSyn R.HTML a] -> VSyn R.HTML a
div_ children = view [R.VNode "div" mempty []] children

runView :: VSyn v a -> Syn.Syn () (Either a ([ViewPatch v], VSyn v a))
runView (VSyn vp) = Syn.local' (<>) $ \e -> do
  (r, (ks, _)) <- Syn.orr' (Left <$> R.runReaderT vp ([], e)) (Right <$> Syn.await e)
  case r of
    Left a  -> pure (Left a)
    Right v -> pure (Right (v, liftSyn (left <$> ks)))

  where
    left (Left a) = a

v e = do
  div_ [ div_ [ text "bla", await e ] ]
  div_ [ text "Done" ]

testView = Syn.local $ \g -> do
  r <- runView (v g)
  case r of
    Left a -> pure (Left a)
    Right (v', next) -> do
      r <- Syn.orr [Left <$> runView next, Right <$> (Syn.emit g 7 >> Syn.forever)]
      case r of
        Left (Left a') -> pure (Left a')
        Left (Right (v'', next')) -> pure (Right (map fst v'))
        Right a -> pure (Left ())
