{-# LANGUAGE OverloadedStrings #-}

module Replica.SVG.Props where

import Data.Text (Text)
import Replica.Props (Props, textProp)

version :: Text -> Props a
version = textProp "version"

xmlns :: Props a
xmlns  = textProp "xmlns" "http://www.w3.org/2000/svg"

d :: Text -> Props a
d = textProp "d"

fill :: Text -> Props a
fill = textProp "fill"

stroke :: Text -> Props a
stroke = textProp "stroke"

strokeWidth :: Text -> Props a
strokeWidth = textProp "stroke-width"

x :: Text -> Props a
x = textProp "x"

y :: Text -> Props a
y = textProp "y"

cx :: Text -> Props a
cx = textProp "cx"

cy :: Text -> Props a
cy = textProp "cy"

r :: Text -> Props a
r = textProp "r"
