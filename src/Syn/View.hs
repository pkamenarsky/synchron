{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings           #-}

module Syn.View where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader as R

import qualified Data.Text                as T

import qualified Replica.VDOM             as R

import qualified Syn as Syn

newtype VSyn v a = VSyn (R.ReaderT ([Int], Syn.Event Syn.Internal ([Int], v)) (Syn.Syn ()) a)
  deriving (Functor, Applicative, Monad)

liftSyn :: Syn.Syn () a -> VSyn v a
liftSyn  = VSyn . lift

mapView :: (u -> v) -> VSyn u a -> VSyn v a
mapView f (VSyn p) = VSyn $ R.ReaderT $ \(path, e') -> Syn.local $ \e -> do
  go path e e'
  where
    go path e e' = do
      (r, _, ks) <- Syn.unsafeRunOrr $ mconcat
        [ Left  <$> Syn.liftOrr (R.runReaderT p (path, e))
        , Right <$> Syn.liftOrr (Syn.await e)
        ]
      case r of
        Left a  -> pure a
        Right (path', a) -> do
          Syn.emit e' (path, (f a))
          go path e e'

await :: Syn.Event t a -> VSyn v a
await = VSyn . lift . Syn.await

emit :: Syn.Event Syn.Internal a -> a -> VSyn v ()
emit e a = VSyn (lift (Syn.emit e a))

view :: v -> [VSyn v a] -> VSyn v a
view v children = VSyn $ R.ReaderT $ \(path, e) -> do
  Syn.orr $ mconcat
    [ [ Syn.emit e (path, v) >> Syn.forever ]
    , [ R.runReaderT child (index:path, e)
      | (VSyn child, index) <- zip children [0..]
      ]
    ]

text :: T.Text -> VSyn R.HTML a
text txt = view [R.VText txt] []

div_ :: [VSyn R.HTML a] -> VSyn R.HTML a
div_ children = view [R.VNode "div" mempty []] children

-- runView :: Show v => VSyn v a -> Syn.Syn () (v, a)
-- runView (VSyn vp) = Syn.local $ \e -> Syn.pool $ \p -> do
--   R.runReaderT vp ([], e)

-- testView = Syn.local $ \v -> Syn.pool $ \p -> do
--   div_ [ div_ [ text "asd" ] ]
--   undefined
