{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TupleSections               #-}

module Syn.View where

import Control.Applicative
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Reader as R

import Data.Bifunctor (second)
import Data.Maybe (fromJust)

import qualified Data.Text                as T

import qualified Replica.VDOM             as R

import qualified Syn as Syn

-- REMOVE
import Data.Typeable
import Debug.Trace

type Path = [Int]

type ViewPatch v = (Path, v)

newtype VSyn v a = VSyn { getVSyn :: R.ReaderT (Path, Syn.Event Syn.Internal [ViewPatch v]) (Syn.Syn ()) a }
  deriving (Functor, Applicative, Monad)

instance Alternative (VSyn v) where
  empty = forever
  VSyn a <|> VSyn b = VSyn $ R.ReaderT $ \(path, e) -> R.runReaderT a (path, e) <|> R.runReaderT b (siblingPath path, e)

-- TODO: this is atrocious
siblingPath :: Path -> Path
-- siblingPath p = init p <> [last p + 1]
siblingPath (p:ps) = (p + 1):ps
  
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

forever :: VSyn v a
forever = liftSyn Syn.forever

await :: Syn.Event t a -> VSyn v a
await = liftSyn . Syn.await

emit :: Syn.Event Syn.Internal a -> a -> VSyn v ()
emit e a = liftSyn (Syn.emit e a)

local :: VSyn v (Syn.Event Syn.Internal a)
local = liftSyn $ Syn.local pure

orr' :: VSyn v a -> VSyn v a -> VSyn v (a, VSyn v a)
orr' (VSyn a) (VSyn b) = VSyn $ R.ReaderT $ \(path, e) -> do
  (a, (ks, _)) <- Syn.orr' (R.runReaderT a (path, e)) (R.runReaderT b (siblingPath path, e))
  pure (a, liftSyn ks)

orr :: [VSyn v a] -> VSyn v a
orr [a] = a
orr [a, b] = fst <$> orr' a b
orr (a:as) = orr [a, orr as]

data Orr v a = Orr (VSyn v (a, Orr v a)) | D

runOrr :: Orr v a -> Maybe (VSyn v (a, Orr v a))
runOrr (Orr o) = Just o
runOrr D = Nothing

instance Semigroup (Orr v a) where
  D <> q = q
  p <> D = p
  Orr p <> Orr q = Orr $ do
    ((a, m), n) <- orr' p q
    pure (a, m <> Orr n)

instance Monoid (Orr v a) where
  mempty = D

liftOrr :: VSyn v a -> Orr v a
liftOrr p = Orr ((,D) <$> p)

view :: Monoid v => v -> [VSyn v a] -> VSyn v a
view v children = VSyn $ R.ReaderT $ \(path, e) -> do
  a <- Syn.orr $ mconcat
    [ [ Syn.emit e [(path, v)] >> Syn.forever ]
    , [ R.runReaderT child (index:path, e)
      | (VSyn child, index) <- zip children [0..]
      ]
    ]
  -- Syn.emit e [(path, mempty)]
  pure a

data Pool v = Pool (Syn.Event Syn.Internal (VSyn v ()))

pool :: (Pool v -> VSyn v a) -> VSyn v a
pool f = do
  e <- local
  go e $ mconcat
    [ liftOrr (Right . Left <$> await e)
    , liftOrr (Left <$> f (Pool e))
    ]
  where
    go e k = do
      (r, k') <- orr [ fromJust (runOrr k) ]

      case r of
        Left a -> pure a
        Right (Left p)  -> go e $ mconcat
          [ liftOrr (Right . Left <$> await e)
          , liftOrr (fmap (Right . Right) p)
          , k'
          ]
        Right (Right _) -> go e k'

spawn :: Pool v -> VSyn v () -> VSyn v ()
spawn (Pool e) p = do
  emit e p

text_ :: T.Text -> VSyn R.HTML a
text_ txt = view [R.VText txt] []

div_ :: [VSyn R.HTML a] -> VSyn R.HTML a
div_ children = view [R.VNode "div" mempty []] children

runView
  :: Syn.Event Syn.Internal [ViewPatch v]
  -> VSyn v a
  -> Syn.Syn () (Either a ([ViewPatch v], VSyn v a))
runView e (VSyn vp) = do
  (r, (ks, _)) <- Syn.orr' (Right <$> Syn.await e) (Left <$> R.runReaderT vp ([0], e))
  case r of
    Left a  -> trace ("runView: Left: " <> show (R.runReaderT vp ([0], e))) $ pure (Left a)
    Right v -> pure (Right (v, liftSyn (left <$> ks)))

  where
    left (Left a) = a

replayView :: VSyn v a -> [Syn.EventValue] -> Syn.Syn () (Maybe a, [[ViewPatch v]])
replayView v es = Syn.local $ \dummy -> Syn.local' (<>) $ \e -> go e [] v (es <> [Syn.EventValue dummy ()])
  where
    go :: Syn.Event Syn.Internal [ViewPatch v] -> [[ViewPatch v]] -> VSyn v a -> [Syn.EventValue] -> Syn.Syn () (Maybe a, [[ViewPatch v]])
    go ve vps _ [] = pure (Nothing, vps)
    go ve vps v (e:es) = Syn.local $ \dummy -> do
      (r, (ks, _)) <- Syn.orr' (Left <$> runView ve v) (Right <$> (Syn.emitValue e))
      case r of
        Left (Left a) -> pure (Just a, vps)
        Left (Right (vp, next)) -> go ve (vp:vps) next es
        Right _ -> go ve vps (liftSyn (left . left <$> ks)) es

    left (Left a) = a

v e = do
  div_ [ div_ [ text_ "bla", v2 ] ]
  div_ [ text_ "Done", await e ]
  div_ [ text_ "Done" ]
  where
    v2 = do
      div_ [ await e ]
      div_ [ await e ]
      div_ [ await e ]

testView = Syn.local $ \d -> Syn.local $ \g -> fmap (fmap (fmap (fmap fst))) $ do
  replayView (v g)
    [ Syn.EventValue g 5
    , Syn.EventValue g 6
    , Syn.EventValue g 6
    , Syn.EventValue g 6
    , Syn.EventValue g 6
    ]
