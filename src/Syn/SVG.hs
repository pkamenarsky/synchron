{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}

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

data TSyn w
  = TDone w
  | TBlocked w
  | TForever w
  | TAwait w Syn.EventId (TSyn w)
  | TEmit w Syn.EventId (TSyn w)
  | TJoin (TSyn w)
  | TBin w BinOp (TSyn w) (TSyn w) (TSyn w)
  deriving (Eq, Show, Functor)

wrapG :: GSyn -> TSyn ()
wrapG GDone = TDone ()
wrapG (GAwait e) = TAwait () e (TDone ())
wrapG (GEmit e) = TEmit () e (TDone ())
wrapG GForever = TForever ()
wrapG (GBin op p q) = TBin () op (wrapG p) (wrapG q) (TDone ())

toTSyn' :: DbgSyn -> TSyn ()
toTSyn' DbgDone = TDone ()
toTSyn' DbgBlocked = TBlocked ()
toTSyn' (DbgAwait e next) = TAwait () e (toTSyn' next)
toTSyn' (DbgEmit e next) = TEmit () e (toTSyn' next)
toTSyn' (DbgJoin next) = TJoin (toTSyn' next)
toTSyn' DbgForever = TForever ()
toTSyn' (DbgBin op p q next) = TBin () (toTOp op) (toTSyn' (p DbgBlocked)) (toTSyn' (q DbgBlocked)) (toTSyn' next)
  where
    toTOp DbgAnd = GAnd
    toTOp DbgOr = GOr

r c x = take x (repeat c)
ss = r ' '

fst3 (a, b, c) = a

color :: Int -> String -> String
color c s = "\ESC[" <> show (31 + (c `mod` 7)) <> "m" <> s <> "\ESC[m"

evColor :: EventId -> String -> String
evColor (Internal (_, c)) = color c
evColor (External (_, c)) = color c

type W = Int
type H = Int

type Pictogram = String

data G = L W H Pictogram | B BinOp Bool W H G G | E W H deriving Show

gw :: G -> W
gw (L w _ _) = w
gw (E w _) = w
gw (B _ _ w _ _ _) = w

gh :: G -> H
gh (L _ h _) = h
gh (E _ h) = h
gh (B _ _ _ h _ _) = h

roww = 2

drawG :: G -> [String]
drawG (L w h c) = r (ss w) (h - 1) <> [c <> ss (w - 1)]
drawG (E w h) = r (ss w) h
drawG (B op draw w h p q) = (if draw then header' else []) <> ls -- zipWith (<>) (drawG p) (drawG q)
  where
    ls =
      [ fromMaybe (ss (gw p)) l <> fromMaybe (ss (gw q)) r
      | (l, r) <- zipPadF (drawG p) (drawG q)
      ]

    top GAnd = "∧"
    top GOr = "∨"

    header = [ top op <> ss (w - 1), r '—' (w - 1) <> [' '] ]
    header' = [ top op <> [' '] <> r '—' (w - 3) <> [' '] ]

drawGs :: [G] -> [String]
drawGs = concatMap drawG

pprintG :: [G] -> IO ()
pprintG = void . traverse putStrLn . drawGs

labelWidth :: W -> TSyn () -> (TSyn W, W) -- returned W >= passed W
labelWidth parw (TDone _) = (TDone parw, parw)
labelWidth parw (TBlocked _) = (TBlocked parw, parw)
labelWidth parw (TForever _) = (TForever parw, parw)
labelWidth parw (TAwait _ e next) = (TAwait w e p, w)
  where
    (p, w) = labelWidth parw next
labelWidth parw (TEmit _ e next) = (TEmit w e p, w)
  where
    (p, w) = labelWidth parw next
labelWidth parw (TBin _ op p q d) = (TBin dw op p' q' d', dw)
  where
    (p', pw) = labelWidth roww p
    (q', qw) = labelWidth roww q
    (d', dw) = labelWidth (max (pw + qw) parw) d
labelWidth parw (TJoin p) = (TJoin p', w)
  where
    (p', w) = labelWidth parw p

toG :: Monoid v => Syn v a -> [G]
toG p = go l
  where
    t = toTSyn' (toDbgSyn p DbgDone)
    l = (,True) <$> fst (labelWidth roww t)

    go g = case showG g of
      (g', Just n) -> g':go n
      (g', _) -> [g']

testG (g:gs) = all ((== gw g) . gw) (g:gs)

showG :: TSyn (W, Bool) -> (G, Maybe (TSyn (W, Bool)))
showG (TDone (w, _)) = (L w 1 "◆", Nothing)
-- showG (TDone (w, _)) = (L w 1 "D", Nothing)
showG (TBlocked (w, _)) = (E w 1, Nothing)
showG (TForever (w, _)) = (L w 1 "∞", Nothing)
-- showG (TAwait (w, _) e next) = (L w 1 ("A"), Just next)
showG (TAwait (w, _) e next) = (L w 1 ("○"), Just next)
-- showG (TEmit (w, _) e next) = (L w 1 ("E"), Just next)
showG (TEmit (w, _) e next) = (L w 1 ("▲"), Just next)
showG (TJoin next) = showG next
showG (TBin (w, draw) op p q d) = case (pg, qg) of
  (E _ _, E _ _) -> showG d
  otherwise -> (pq, Just (TBin (w, False) op (fromMaybe (TBlocked (gw pg, False)) p') (fromMaybe (TBlocked (gw qg, False)) q') d))
  where
    (pg, p') = showG p
    (qg, q') = showG q

    adjustH :: Int -> G -> G
    adjustH h (L w _ c) = L w h c
    -- only adjust height if no header drawn
    adjustH h (B op False bw _ p q) = B op False bw h (adjustH h p) (adjustH h q)
    adjustH _ b = b

    mh = max (gh pg) (gh qg)

    pq = B op draw w (mh + if draw then 1 else 0) (adjustH mh pg) (adjustH mh qg)

showTSyn :: TSyn () -> ([[String]], Int)
showTSyn (TDone _) = ([["◆"]], 1)
showTSyn (TBlocked _) = ([], 1)
showTSyn (TForever _) = ([["∞"]], 1)
showTSyn (TJoin next) = (t, w) -- ([["v" <> ss (w - 1)]] <> t, w)
  where
    (t, w) = showTSyn next
showTSyn (TAwait _ e next) = ([[evColor e "○" <> ss (w - 1)]] <> t, w)
  where
    (t, w) = showTSyn next
showTSyn (TEmit _ e next) = ([[evColor e "▲" <> ss (w - 1)]] <> t, w)
  where
    (t, w) = showTSyn next
showTSyn (TBin _ op p q d) =
  ( header <> go pg (fmap (fmap (fmap (padto qw ' '))) qg) <> dt
  , pw + qw + 1
  )
  where
    (pt, pw) = showTSyn p
    (qt, qw') = showTSyn q
    (dt, dw) = showTSyn d

    qw = max qw' dw

    padto x r s = take (x - length s) (repeat r) <> s

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
    convert (Syn (Free (Await (Event _ e) _))) = GAwait e
    convert (Syn (Free (Emit (EventValue (Event _ e) _) _))) = GEmit e
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

pprintProgram p = void $ traverse putStrLn (showProgram' ((reverse $ toGSyn p)))

pprintProgram2 p = void $ traverse putStrLn (concat $ fst $ showTSyn (toTSyn' (toDbgSyn p DbgDone)))

pprintProgram3 p = fst $ labelWidth roww $ toTSyn' (toDbgSyn p DbgDone)

pprintProgram4 :: Monoid v => Syn v a -> IO ()
pprintProgram4 = pprintG . toG

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

trail' :: String -> Text -> Double -> Double -> Double -> Double -> Double -> Element
trail' c color x y width height r = mconcat
  [ path_
      [ D_ <<- (mA x y <> vR height <> aR (width / 2) (width /2) 0 1 0 width 0 <> vR (-height) <> z)
      , Fill_ <<- color
      ]
  , case c of
      "○" -> circle_
        [ Cx_ <<- t (x + (width / 2))
        , Cy_ <<- t (y + height)
        , R_ <<- t (r / 1.2)
        , Stroke_ <<- "#fff"
        , Stroke_width_ <<- "2px"
        , Fill_ <<- "transparent"
        , Class_ <<- "circle"
        ]
      "◆" -> path_
        [ D_ <<- (mA (x + width / 2) (y + height - r) <> lR r r <> lR (-r) r <> lR (-r) (-r) <> z)
        , Fill_ <<- "#fff"
        ]
      "▲" -> path_
        [ D_ <<- (mA (x + width / 2 - r) (y + height - r) <> lR r (r * 1.8) <> lR r (-r * 1.8) <> z)
        , Fill_ <<- "#fff"
        ]
      otherwise -> path_ []
  ]
  where
    t = T.pack . show

trail :: Text -> Double -> Double -> Double -> Double -> Double -> Element
trail color x y width height r = mconcat
  [ path_
      [ D_ <<- (mA x y <> vR height <> aR (width / 2) (width /2) 0 1 0 width 0 <> vR (-height) <> z)
      , Fill_ <<- color
      ]
  , mconcat
      [ circle_
          [ Cx_ <<- t (x + (width / 2))
          , Cy_ <<- t (y + height)
          , R_ <<- t r
          , Fill_ <<- "#ddd"
          , Class_ <<- "circle"
          ]
      ]
  ]
  where
    t = T.pack . show

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

gridw = 16
gridh = 30
gridx = 50
gridy = 50

tox x = gridx + fromIntegral x * gridw
toy y = gridy + fromIntegral y * gridh

tow w = fromIntegral w * gridw
toh h = fromIntegral h * gridh

gridTrail :: String -> Int -> Int -> Int -> Element
gridTrail t x y h = trail' t "#333" (tox x) (toy y) (tow 1) (toh h) (gridw / 4)

svgG :: Int -> Int -> G -> Element
svgG x y (L _ h c) = gridTrail c x y h
svgG x y (E _ _) = mempty
svgG x y (B op draw w h p q) = mconcat
  [ svgG x (y + h - gh p) p
  , svgG (x + gw p) (y + h - gh q) q
  , if draw
      then path_
        -- [ D_ <<- (mA (tox x) (toy (y + 1) - (gridh / 3)) <> hR (tow (w - 1)))
        [ D_ <<- (mA (tox x) (toy (y + 1) - (gridh / 3) - 2) <> lR tm 0 <> lR tx (-ty) <> lR tx ty <> lR tm 0) 
        , Stroke_width_ <<- "3px"
        , Stroke_ <<- "#333"
        , Fill_ <<- "transparent"
        ]
      else mempty
  ]
  where
    tw = tow (w - 1)
    tx = 6
    ty = case op of
      GAnd -> 6
      GOr -> (-6)

    tm = tw / 2 - tx

svgGs :: H -> [G] -> Element
svgGs _ [] = mempty
svgGs h (g:gs) = svgGs (h + h') gs <> svgG 0 h g
  where
    h' = gh g

makeSvg :: Monoid v => Syn v a -> IO ()
makeSvg p = writeFile "out.svg" (generateSVG "" (svgGs 0 $ toG p))

-- main :: IO ()
-- main = do
--   style <- readFile "style.css"
--   writeFile "out.svg" (generateSVG style trails)
