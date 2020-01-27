{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Syn.SVG.Replica where

import Control.Applicative (empty, (<|>))

import Data.Text (Text)
import qualified Data.Text as T

import Graphics.Svg hiding (aR)

import           Replica.DOM (HTML)
import           Replica.Props
import           Replica.SVG
import           Replica.SVG.Props

import           Syn (orr, Syn)
import           Syn.SVG (Pictogram (..), paletteColor, G (..), BinOp (..), H, W, toG)

import           Prelude hiding (div)

aR :: RealFloat a =>  a -> a -> a -> Int -> Int -> a -> a -> Text
aR rx ry xrot largeFlag sweepFlag x y = T.concat
  [ "a ", toText rx, ",", toText ry, " ", toText xrot, " ", T.pack (show largeFlag)
  , " ", T.pack (show sweepFlag), " ", toText x, " ", toText y, " "]

container = svg
  [ width "500", height "500", version "1.1", xmlns ]
  [ rect [ x "20", y "20", width "100", height "100", fill "#333" ] [] ]

trail :: Pictogram -> Text -> Double -> Double -> Double -> Double -> Double -> Syn HTML ()
trail c color x y width height radius = orr
  [ path [ d (mA x y <> vR height <> aR (width / 2) (width /2) 0 1 0 width 0 <> vR (-height) <> z)
         , fill "#000"
         ] []
  , case c of
      PictAwait c -> circle
        [ cx (t (x + (width / 2)))
        , cy (t (y + height))
        , r (t (radius / 1.1))
        -- , stroke (paletteColor c)
        -- , strokeWidth "2px"
        -- , fill "transparent"
        , fill (paletteColor c)
        , className "circle"
        ] []
      PictDone -> path
        [ d (mA (x + width / 2) (y + height - radius) <> lR radius radius <> lR (-radius) radius <> lR (-radius) (-radius) <> z)
        , fill "#fff"
        ] []
      PictEmit c -> path
        [ d (mA (x + width / 2 - radius) (y + height - radius) <> lR radius (radius * 1.8) <> lR radius (-radius * 1.8) <> z)
        , fill (paletteColor c)
        ] []
      otherwise -> path [] []
  ]
  where
    t = T.pack . show

gridw = 12
gridh = 30
gridx = 50
gridy = 50

tox x = gridx + fromIntegral x * gridw
toy y = gridy + fromIntegral y * gridh

tow w = fromIntegral w * gridw
toh h = fromIntegral h * gridh

gw :: G -> W
gw (L w _ _) = w
gw (E w _) = w
gw (B _ _ w _ _ _) = w

gh :: G -> H
gh (L _ h _) = h
gh (E _ h) = h
gh (B _ _ _ h _ _) = h

roww = 2

gridTrail :: Pictogram -> Int -> Int -> Int -> Syn HTML ()
gridTrail t x y h = trail t "#333" (tox x) (toy y) (tow 1) (toh h) (gridw / 4)

svgG :: Int -> Int -> G -> Syn HTML ()
svgG x y (L _ h c) = gridTrail c x y h
svgG x y (E _ _) = empty
svgG x y (B op draw w h p q) = orr
  [ svgG x (y + h - gh p) p
  , svgG (x + gw p) (y + h - gh q) q
  , if draw
      then path
        -- [ d (mA (tox x) (toy (y + 1) - (gridh / 3)) <> hR (tow (w - 1)))
        [ d (mA (tox x) (toy (y + 1) - (gridh / 3) - 2) <> lR tm 0 <> lR tx (-ty) <> lR tx ty <> lR tm 0) 
        , strokeWidth "3px"
        , stroke "#000"
        , fill "transparent"
        ] []
      else empty
  ]
  where
    tw = tow (w - 1)
    tx = 6
    ty = case op of
      GAnd -> 6
      GOr -> (-6)

    tm = tw / 2 - tx

svgGs :: H -> [G] -> Syn HTML ()
svgGs _ [] = empty
svgGs h (g:gs) = svgGs (h + h') gs <|> svgG 0 h g
  where
    h' = gh g

synSvg :: Monoid v => Syn v a -> Syn HTML ()
synSvg = svgGs 0 . toG

synSvg' :: Syn () a -> Syn HTML ()
synSvg' = svgGs 0 . toG

synSvgApp p = svg
  [ width "1000", height "1000", version "1.1", xmlns ]
  [ synSvg' p ]
