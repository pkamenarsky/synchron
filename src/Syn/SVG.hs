{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Syn.SVG where

import Control.Monad.Free

import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T

import Graphics.Svg hiding (aR)

import Syn (Syn (..), SynF (..))
import qualified Syn

import Debug.Trace

data BinOp = GAnd | GOr deriving (Eq, Show)

data GSyn
  = GDone
  | GAwait
  | GEmit
  | GForever
  | GBin BinOp GSyn GSyn
  deriving (Eq, Show)

data TSyn
  = TDone
  | TAwait TSyn
  | TEmit TSyn
  | TForever
  | TBin BinOp TSyn TSyn
  deriving (Eq, Show)

wrapG :: GSyn -> TSyn
wrapG GDone = TDone
wrapG GAwait = TAwait TDone
wrapG GEmit = TEmit TDone
wrapG GForever = TForever
wrapG (GBin op p q) = TBin op (wrapG p) (wrapG q)

-- | Must be right folded
match :: TSyn -> TSyn -> TSyn
match TDone u = u
match (TAwait t) u = TAwait (match t u)
match (TEmit t) u = TEmit (match t u)
match TForever _ = TForever
match (TBin op p q) x@(TBin op' p' q')
  | op == op' = TBin op (match p p') (match q q')
match (TBin op p q) u = TBin op (match p u) q

toTSyn :: [GSyn] -> TSyn
toTSyn [] = error ""
toTSyn [g] = wrapG g
toTSyn (g:gs) = match (wrapG g) (toTSyn gs)

r c x = take x (repeat c)
ss = r ' '

fst3 (a, b, c) = a

showTSyn :: TSyn -> ([[String]], Int)
showTSyn TDone = ([["◆"]], 1)
showTSyn TForever = ([["∞"]], 1)
showTSyn (TAwait next) = ([["○" <> ss (w - 1)]] <> t, w)
  where
    (t, w) = showTSyn next
showTSyn (TEmit next) = ([["▲" <> ss (w - 1)]] <> t, w)
  where
    (t, w) = showTSyn next
showTSyn (TBin op p q) =
  ( header <> go pg qg
  , pw + qw + 1
  )
  where
    (pt, pw) = showTSyn p
    (qt, qw) = showTSyn q
    (pg, qg) = unzip (zipPadB pt qt)

    top GAnd = "∧"
    top GOr = "∨"

    header' =
      [ [ top op <> " " <> r '—' (pw + qw + 1 - 2)
        ]
      ]

    header =
      [ [ top op <> ss (pw + qw + 1 - 1)
        , r '—' (pw + qw + 1)
        ]
      ]

    go :: [Maybe [String]] -> [Maybe [String]] -> [[String]]
    -- go a b
    --   | trace (show (a, b)) False = undefined
    go [] _ = []
    go _ [] = []
    go (Just [pt]:ps) (Just [qt]:qs) = [[pt <> " " <> qt]] <> go ps qs
    go (Just [pt]:ps) (Nothing:qs) = [[pt <> " " <> ss qw]] <> go ps qs
    go (Nothing:ps) (Just [qt]:qs) = [[ss pw <> " " <> qt]] <> go ps qs
    go (Just pt:ps) x@(Just [qt]:qs) = [map (<> (" " <> ss qw)) pt] <> go ps x
    go (Just pt:ps) x@(Nothing:qs) = [map (<> (" " <> ss qw)) pt] <> go ps x
    go x@(Just [pt]:ps) (Just qt:qs) = [map ((" " <> ss pw) <>) qt] <> go x qs
    go x@(Nothing:ps) (Just qt:qs) = [map ((" " <> ss pw) <>) qt] <> go x qs
    go (Just pt:ps) (Just qt:qs) = [ls] <> go ps qs
      where
        ls =
          [ case t of
              (Just a, Just b) -> a <> " " <> b
              (Just a, Nothing) -> a <> " " <> ss qw
              (Nothing, Just b) -> ss pw <> " " <> b
          | t <- zipPadF pt qt
          ]

toGSyn :: Monoid v => Syn v a -> [GSyn]
toGSyn = go mempty 0 []
  where
    convert :: Syn v a -> GSyn
    convert (Syn (Pure _)) = GDone
    convert (Syn (Free Forever)) = GForever
    convert (Syn (Free (MapView _ p _))) = convert p
    convert (Syn (Free (Await _ _))) = GAwait
    convert (Syn (Free (Emit _ _))) = GEmit
    convert (Syn (Free (Or p q _))) = GBin GOr (convert p) (convert q)
    convert (Syn (Free (And p q _))) = GBin GAnd (convert p) (convert q)

    go m eid gp p = if M.size m' == 0
      then (convert p':gp)
      else go m' eid' (convert p':gp) p'
      where
        (eid', p', _, m', ios, u) = Syn.stepOnce' m 0 eid p Syn.E

zipPadB :: [a] -> [b] -> [(Maybe a, Maybe b)]
zipPadB as bs = zip
  (map Just as <> take (max 0 (length bs - length as)) (repeat Nothing))
  (map Just bs <> take (max 0 (length as - length bs)) (repeat Nothing))

zipPadF :: [a] -> [b] -> [(Maybe a, Maybe b)]
zipPadF as bs = zip
  (take (max 0 (length bs - length as)) (repeat Nothing) <> map Just as)
  (take (max 0 (length as - length bs)) (repeat Nothing) <> map Just bs)

showTrail :: GSyn -> [String]
showTrail = fst3 . go
  where
    go :: GSyn -> ([String], Int, Int)
    go GEmit = (["▲"], 1, 0)
    go GAwait = (["○"], 1, 0)
    go GDone = (["◆"], 1, 0)
    go GForever = (["∞"], 1, 0)
    go (GBin op p q) =
      ( header <> subheader <> lines
      , pw + qw + 1
      , length header + length subheader
      )
      where
        top GAnd = "∧"
        top GOr = "∨"

        header' =
          [ top op <> " " <> r '—' (pw + qw + 1 - 2)
          ]

        header =
          [ top op <> ss (pw + qw + 1 - 1)
          , r '—' (pw + qw + 1)
          ]

        subheader =
          [ mconcat
             [ case ph of
                 Nothing -> ss pw
                 Just t  -> t
             , " "
             , case qh of
                 Nothing -> ss qw
                 Just t  -> t
             ]
          | (ph, qh) <- zipPadF (take ph pt) (take qh qt)
          ]

        lines =
          [ mconcat
             [ case ph of
                 Nothing -> ss pw
                 Just t  -> t
             , " "
             , case qh of
                 Nothing -> ss qw
                 Just t  -> t
             ]
          | (ph, qh) <- zipPadF (drop ph pt) (drop qh qt)
          ]
        (pt, pw, ph) = go p
        (qt, qw, qh) = go q

showProgram :: [GSyn] -> [String]
showProgram = concat . go []
  where
    go u [] = []
    go u (p:ps) = t':go t ps
      where
        t = showTrail p
        t' = strip u t

    strip [] bs = bs
    strip (a:as) (b:bs)
      | a == b = strip as bs
      | otherwise = (b:bs)

--------------------------------------------------------------------------------

p4 :: Syn () _
p4 = Syn.local $ \e -> Syn.local $ \f -> do
  a@((_, _), _, (_, _)) <- Syn.andd
         ( Syn.andd (Syn.await e, Syn.emit f "F")
         , Syn.orr [ Syn.await f >> Syn.orr [ Syn.forever, Syn.await e ], Syn.forever ]
         , Syn.andd (pure "_" :: Syn () String, Syn.await f >> Syn.emit e "E")
         )
  pure a

svg :: Element
svg = g_ [] (text_ [] "YO")

p :: Element
p = path_
  [ D_ <<- (mA 10 80 <> qA 52.5 10 95 80 <> tA 180 80 <> z)
  , Fill_ <<- "#333333"
  ]

aR :: RealFloat a =>  a -> a -> a -> Int -> Int -> a -> a -> Text
aR rx ry xrot largeFlag sweepFlag x y = T.concat
  [ "a ", toText rx, ",", toText ry, " ", toText xrot, " ", T.pack (show largeFlag)
  , " ", T.pack (show sweepFlag), " ", toText x, " ", toText y, " "]

trail :: Text -> Double -> Double -> Double -> Double -> Double -> Element
trail color x y width height coffset = mconcat
  [ path_
      [ D_ <<- (mA x y <> vR height <> aR (width / 2) (width /2) 0 1 0 width 0 <> vR (-height) <> z)
      , Fill_ <<- color
      ]
  , mconcat
      [ circle_
          [ Cx_ <<- t (x + (width / 2))
          , Cy_ <<- t (y + yo + 5 + (width / 2))
          , R_ <<- t (width / 2 - coffset)
          , Fill_ <<- "#ddd"
          , Class_ <<- "circle"
          ]
      | yo <- [0,16..height-20]
      ]
  ]
  where
    t = T.pack . show

dtrail :: Text -> Double -> Double -> Double -> Element
dtrail color x y height = mconcat
  [ trail color x y 29 height 10
  , trail "#ddd" (x + 3) (y + 3) 10 (height - 20) 10
  , trail "#ddd" (x + 16) (y + 3) 10 (height - 50) 10
  ]

trails =
  [ trail "#333" (100 + x * 42) 100 14 (height * 40) 5
  -- [ dtrail "#333" (100 + x * 60) 100 (height * 60)
  | (x, height) <- zip [0..10] [4, 5, 7, 2, 6, 12, 8, 11, 9, 4, 6, 10 ]
  ]

generateSVG style t = mconcat $ intersperse "\n"
  [ "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>"
  , "<svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" x=\"0px\" y=\"0px\" viewBox=\"0 0 960 960\">"
  , "<defs>"
  , "<style type=\"text/css\"><![CDATA["
  , style
  , "]]></style>"
  , "</defs>"
  , show t
  , "</svg>"
  ]

main :: IO ()
main = do
  style <- readFile "style.css"
  writeFile "out.svg" (generateSVG style trails)
