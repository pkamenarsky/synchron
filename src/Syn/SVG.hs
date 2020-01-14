{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Syn.SVG where

import Control.Monad.Free

import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T

import Graphics.Svg hiding (aR)

import Syn (Syn (..), SynF (..))
import qualified Syn

import Debug.Trace

data BinOp = GAnd | GOr deriving Show

data GSyn
  = GDone
  | GAwait
  | GEmit
  | GForever
  | GBin BinOp GSyn GSyn
  deriving Show

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

printTrails :: [GSyn] -> [String]
printTrails = concatMap (fst3 . go 0)
  where
    fst3 (a, b, c) = a

    r c x = take x (repeat c)
    ss = r ' '

    go :: Int -> GSyn -> ([String], Int, Int)
    go x GEmit = ([ss x <> "E"], 1, 0)
    go x GAwait = ([ss x <> "A"], 1, 0)
    go x GDone = ([ss x <> "D"], 1, 0)
    go x GForever = ([ss x <> "F"], 1, 0)
    go x (GBin op p q) =
      ( header <> subheader <> lines
      , pw + qw + 1
      , length header + length subheader
      )
      where
        top GAnd = "&&"
        top GOr = "||"

        header =
          [ ss x <> top op <> ss (pw + qw + 1 - 2)
          , ss x <> r '-' (pw + qw + 1)
          ]
        subheader =
          [ mconcat
             [ ss x
             , case ph of
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
             [ ss x
             , case ph of
                 Nothing -> ss pw
                 Just t  -> t
             , " "
             , case qh of
                 Nothing -> ss qw
                 Just t  -> t
             ]
          | (ph, qh) <- zipPadF (drop ph pt) (drop qh qt)
          ]
        (pt, pw, ph) = go 0 p
        (qt, qw, qh) = go 0 p

p4 :: Syn () _
p4 = Syn.local $ \e -> Syn.local $ \f -> do
  a@((_, _), _, (_, _)) <- Syn.andd
         ( Syn.andd (Syn.await e, Syn.emit f "F")
         , Syn.await f
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
