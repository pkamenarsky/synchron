{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Syn.SVG where

import Control.Monad (void)
import Control.Monad.Free

import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T

import Graphics.Svg hiding (aR)

import Syn (DbgSyn (..), DbgBinOp (..), Syn (..), SynF (..), Event (..), EventId (..), EventValue (..))
import qualified Syn

import Debug.Trace

data BinOp = GAnd | GOr deriving (Eq, Show)

data GSyn
  = GDone
  | GAwait Syn.EventId
  | GEmit Syn.EventId
  | GForever
  | GBin BinOp GSyn GSyn
  deriving (Eq, Show)

data TSyn
  = TDone
  | TBlocked
  | TAwait Syn.EventId TSyn
  | TEmit Syn.EventId TSyn
  | TForever
  | TJoin TSyn
  | TBin BinOp TSyn TSyn TSyn
  deriving (Eq, Show)

wrapG :: GSyn -> TSyn
wrapG GDone = TDone
wrapG (GAwait e) = TAwait e TDone
wrapG (GEmit e) = TEmit e TDone
wrapG GForever = TForever
wrapG (GBin op p q) = TBin op (wrapG p) (wrapG q) TDone

-- | Must be right folded
match :: GSyn -> TSyn -> TSyn
match GDone u = u
match (GAwait e) u = TAwait e u
match (GEmit e) u = TEmit e u
match GForever _ = TForever
match (GBin op p q) x@(TBin op' p' q' d')
  | op == op' = TBin op (match p p') (match q q') d'
match (GBin op p q) u = TBin op (match p u) (wrapG q) u
-- match (GBin op p q) u = TBin op (wrapG p) (wrapG q) u

toTSyn' :: DbgSyn -> TSyn
toTSyn' DbgDone = TDone
toTSyn' DbgBlocked = TBlocked
toTSyn' (DbgAwait e next) = TAwait e (toTSyn' next)
toTSyn' (DbgEmit e next) = TEmit e (toTSyn' next)
toTSyn' (DbgJoin next) = toTSyn' next
toTSyn' DbgForever = TForever
toTSyn' (DbgBin op p q next) = TBin (toTOp op) (toTSyn' (p DbgBlocked)) (toTSyn' (q DbgBlocked)) (toTSyn' next)
  where
    toTOp DbgAnd = GAnd
    toTOp DbgOr = GOr

toTSyn :: [GSyn] -> TSyn
toTSyn [] = error ""
toTSyn [g] = wrapG g
toTSyn (g:gs) = match g (toTSyn gs)

r c x = take x (repeat c)
ss = r ' '

fst3 (a, b, c) = a

color :: Int -> String -> String
color c s = "\ESC[" <> show (31 + (c `mod` 7)) <> "m" <> s <> "\ESC[m"

evColor :: EventId -> String -> String
evColor (Internal (_, c)) = color c
evColor (External (_, c)) = color c

showTSyn :: TSyn -> ([[String]], Int)
-- showTSyn TDone = ([["\ESC[34m◆\ESC[m"]], 1)
showTSyn TDone = ([["◆"]], 1)
showTSyn TBlocked = ([], 1)
-- showTSyn TDone = ([[]], 1)
showTSyn TForever = ([["∞"]], 1)
showTSyn (TAwait e next) = ([[evColor e "○" <> ss (w - 1)]] <> t, w)
  where
    (t, w) = showTSyn next
showTSyn (TEmit e next) = ([[evColor e "▲" <> ss (w - 1)]] <> t, w)
  where
    (t, w) = showTSyn next
showTSyn (TBin op p q d) =
  ( header <> go (fmap (fmap (fmap (padto pw ' '))) pg) qg <> dt
  , pw + qw + 1
  )
  where
    (pt, pw) = showTSyn p
    (qt, qw) = showTSyn q
    (dt, dw) = showTSyn d

    -- pw = max pw' dw

    padto x r s = s <> take (x - length s) (repeat r)

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

zipLines :: [[String]] -> [[String]] -> [(Maybe [String], Maybe [String])]
zipLines ([a]:as) ([b]:bs) = (Just [a], Just [b]):zipLines as bs
zipLines ([a]:as) (b:bs) = (Nothing, Just b):zipLines ([a]:as) bs
zipLines (a:as) ([b]:bs) = (Just a, Nothing):zipLines as ([b]:bs)

toDbgSyn :: Monoid v => Syn v a -> (DbgSyn -> DbgSyn)
toDbgSyn = go mempty 0 id
  where
    go m eid dbg p = if M.size m' == 0
      then dbg . dbg'
      else go m' eid' (dbg . dbg') p'
      where
        (eid', p', dbg', _, m', ios, u) = Syn.stepOnce' m 0 eid p Syn.E

toGSyn :: Monoid v => Syn v a -> [GSyn]
toGSyn = go mempty 0 []
  where
    convert :: Syn v a -> GSyn
    convert (Syn (Pure _)) = GDone
    convert (Syn (Free Forever)) = GForever
    convert (Syn (Free (MapView _ p _))) = convert p
    convert (Syn (Free (Await (Event e) _))) = GAwait e
    convert (Syn (Free (Emit (EventValue (Event e) _) _))) = GEmit e
    convert (Syn (Free (Or p q _))) = GBin GOr (convert p) (convert q)
    convert (Syn (Free (And p q _))) = GBin GAnd (convert p) (convert q)

    go m eid gp p = if M.size m' == 0
      then convert p':gp
      else go m' eid' (convert p':gp) p'
      where
        (eid', p', _, _, m', ios, u) = Syn.stepOnce' m 0 eid p Syn.E

zipPadB :: [a] -> [b] -> [(Maybe a, Maybe b)]
zipPadB as bs = zip
  (map Just as <> take (max 0 (length bs - length as)) (repeat Nothing))
  (map Just bs <> take (max 0 (length as - length bs)) (repeat Nothing))

zipPadF :: [a] -> [b] -> [(Maybe a, Maybe b)]
zipPadF as bs = zip
  (take (max 0 (length bs - length as)) (repeat Nothing) <> map Just as)
  (take (max 0 (length as - length bs)) (repeat Nothing) <> map Just bs)

pprintProgram p = void $ traverse putStrLn (concat $ fst $ showTSyn (toTSyn  (reverse $ toGSyn p)))

pprintProgram2 p = void $ traverse putStrLn (showProgram' ((reverse $ toGSyn p)))

pprintProgram3 p = void $ traverse putStrLn (concat $ fst $ showTSyn (toTSyn' (toDbgSyn p DbgDone)))

showTrail :: GSyn -> [String]
showTrail = fst3 . go
  where
    go :: GSyn -> ([String], Int, Int)
    go (GEmit e) = ([evColor e "▲"], 1, 0)
    go (GAwait e) = ([evColor e "○"], 1, 0)
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

showProgram' :: [GSyn] -> [String]
showProgram' = concat . intersperse [""] . map showTrail

--------------------------------------------------------------------------------

-- p4 :: Syn () _
-- p4 = Syn.local $ \e -> Syn.local $ \f -> do
--   a@((_, _), _, (_, _)) <- Syn.andd
--          ( Syn.andd (Syn.await e, Syn.emit f "F")
--          , Syn.orr [ Syn.await f >> Syn.orr [ Syn.forever, Syn.await e ], Syn.forever ]
--          , Syn.andd (pure "_" :: Syn () String, Syn.await f >> Syn.emit e "E")
--          )
--   pure a

-- void $ traverse putStrLn (concat $ fst $ showTSyn (toTSyn  (reverse $ toGSyn p6)))

-- p6 = Syn.local $ \e -> Syn.local $ \f -> Syn.local $ \g -> do
--   a@((_, _), (_, _, _), (_, _, _), (_, _)) <- Syn.andd
--     ( Syn.andd (Syn.await e, Syn.emit f "F" >> (Syn.andd (Syn.await g, Syn.await g) :: Syn () (_, _)) >> Syn.emit e "E")
--     , Syn.andd (Syn.await f, Syn.await g, Syn.await e)
--     , Syn.andd (Syn.await e, Syn.await g, Syn.await f)
--     , Syn.andd (pure "_" :: Syn () String, Syn.await f >> Syn.emit g "G")
--     )
--   pure a
-- 
-- p6_2 = Syn.local $ \e -> Syn.local $ \f -> Syn.local $ \g -> do
--   a@((_, _), (_, _, _), (_, _, _), (_, _)) <- Syn.andd
--     ( Syn.andd (Syn.await e, Syn.emit f "F" >> Syn.await g>> Syn.emit e "E")
--     , Syn.andd (Syn.await f, Syn.await g, Syn.await e)
--     , Syn.andd (Syn.await e, Syn.await g, Syn.await f)
--     , Syn.andd (pure "_" :: Syn () String, Syn.await f >> Syn.emit g "G")
--     )
--   pure a
-- 
-- p7 :: Syn () _
-- p7 = Syn.local $ \e -> Syn.local $ \f -> do
--   (_, _) <- Syn.andd
--     ( go 0 0 e f
--     , do
--         Syn.emit f 1
--         Syn.emit f 2
--         Syn.emit f 3
--         Syn.emit f 6
--         Syn.emit f 8
--         Syn.emit e (Right ())
--     )
--   pure ()
--   where
--     go :: Int -> Int -> Syn.Event Syn.Internal (Either Int ()) -> Syn.Event Syn.Internal Int -> Syn () Int
--     go x y e f = do
--       a <- Syn.orr [ Left <$> Syn.await e, Right <$> Syn.await f ]
--       case a of
--         Left (Left x') -> go (x + x') y e f
--         Right y'       -> go x (y + y') e f
--         _              -> pure (x + y)

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

-- main :: IO ()
-- main = do
--   style <- readFile "style.css"
--   writeFile "out.svg" (generateSVG style trails)
