{-# LANGUAGE OverloadedStrings   #-}

module Replica.SVG where

import Data.Text (Text)
import Replica.VDOM.Types (Namespace(Namespace))

import Replica.Props
import Replica.DOM (el', HTML)

import Syn

el :: Text -> [Props a] -> [Syn HTML a] -> Syn HTML a
el = el' (Just (Namespace "http://www.w3.org/2000/svg"))

svg :: [Props a] -> [Syn HTML a] -> Syn HTML a
svg = el "svg"

rect :: [Props a] -> [Syn HTML a] -> Syn HTML a
rect = el "rect"

circle :: [Props a] -> [Syn HTML a] -> Syn HTML a
circle = el "circle"

path :: [Props a] -> [Syn HTML a] -> Syn HTML a
path = el "path"
