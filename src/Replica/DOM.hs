{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}

module Replica.DOM where

import           Control.Concurrent       (newEmptyMVar, putMVar, takeMVar)
import           Control.Monad            (void)

import           Replica.Props            (Props(Props), Prop(PropText, PropBool, PropEvent, PropMap), key)

import           Data.Bifunctor           (second)
import           Data.Monoid              ((<>))
import qualified Data.Text                as T

import qualified Data.Map                 as M

import           Replica.VDOM             (Attr(AText, ABool, AEvent, AMap), DOMEvent, VDOM(VNode, VText))
import qualified Replica.VDOM             as R

import           Syn

newtype HTML = HTML { runHTML :: Context HTML () -> R.HTML }
  deriving (Semigroup, Monoid)

el :: T.Text -> [Props a] -> [Syn HTML a] -> Syn HTML a
el e attrs children = do
  attrs' <- traverse toAttr attrs
  mapView
    (\children -> HTML $ \ctx -> [VNode e (M.fromList $ fmap (second ($ ctx) . fst) attrs') (runHTML children ctx)])
    (orr (children <> concatMap snd attrs'))
  where
    toAttr :: Props a -> Syn HTML ((T.Text, Context HTML () -> Attr), [Syn HTML a])
    toAttr (Props k (PropText v)) = pure ((k, \_ -> AText v), [])
    toAttr (Props k (PropBool v)) = pure ((k, \_ -> ABool v), [])
    toAttr (Props k (PropEvent extract)) = local $ \e -> do
      pure ((k, \ctx -> AEvent $ \de -> void $ push ctx e de), [extract <$> await e])
    toAttr (Props k (PropMap m)) = do
      m' <- mapM toAttr m
      pure ((k, \ctx -> AMap $ M.fromList $ fmap (second ($ ctx) . fst) m'), concatMap snd m')

text :: T.Text -> Syn HTML a
text txt = view (HTML $ \_ -> [VText txt]) >> forever

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
div :: [Props a] -> [Syn HTML a] -> Syn HTML a
div  = el "div"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
table :: [Props a] -> [Syn HTML a] -> Syn HTML a
table  = el "table"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
thead :: [Props a] -> [Syn HTML a] -> Syn HTML a
thead  = el "thead"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbody :: [Props a] -> [Syn HTML a] -> Syn HTML a
tbody  = el "tbody"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
tr :: [Props a] -> [Syn HTML a] -> Syn HTML a
tr  = el "tr"

-- | Contains `Key`, inteded to be used for child replacement patch
--
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr>

trKeyed :: T.Text -> [Props a] -> [Syn HTML a] -> Syn HTML a
trKeyed k props = el "tr" (key k:props)

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
th :: [Props a] -> [Syn HTML a] -> Syn HTML a
th  = el "th"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
td :: [Props a] -> [Syn HTML a] -> Syn HTML a
td  = el "td"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfoot :: [Props a] -> [Syn HTML a] -> Syn HTML a
tfoot  = el "tfoot"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
section :: [Props a] -> [Syn HTML a] -> Syn HTML a
section  = el "section"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
header :: [Props a] -> [Syn HTML a] -> Syn HTML a
header  = el "header"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footer :: [Props a] -> [Syn HTML a] -> Syn HTML a
footer  = el "footer"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
button :: [Props a] -> [Syn HTML a] -> Syn HTML a
button = el "button"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
form :: [Props a] -> [Syn HTML a] -> Syn HTML a
form = el "form"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
p :: [Props a] -> [Syn HTML a] -> Syn HTML a
p = el "p"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
s :: [Props a] -> [Syn HTML a] -> Syn HTML a
s = el "s"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ul :: [Props a] -> [Syn HTML a] -> Syn HTML a
ul = el "ul"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
span :: [Props a] -> [Syn HTML a] -> Syn HTML a
span = el "span"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strong :: [Props a] -> [Syn HTML a] -> Syn HTML a
strong = el "strong"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
li :: [Props a] -> [Syn HTML a] -> Syn HTML a
li = el "li"

-- | Contains `Key`, inteded to be used for child replacement patch
--
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li>
--
liKeyed :: T.Text -> [Props a] -> [Syn HTML a] -> Syn HTML a
liKeyed k props = el "li" (key k:props)

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1 :: [Props a] -> [Syn HTML a] -> Syn HTML a
h1 = el "h1"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2 :: [Props a] -> [Syn HTML a] -> Syn HTML a
h2 = el "h2"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3 :: [Props a] -> [Syn HTML a] -> Syn HTML a
h3 = el "h3"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4 :: [Props a] -> [Syn HTML a] -> Syn HTML a
h4 = el "h4"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5 :: [Props a] -> [Syn HTML a] -> Syn HTML a
h5 = el "h5"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6 :: [Props a] -> [Syn HTML a] -> Syn HTML a
h6 = el "h6"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hr :: [Props a] -> Syn HTML a
hr = flip (el "hr") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
pre :: [Props a] -> [Syn HTML a] -> Syn HTML a
pre = el "pre"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
input :: [Props a] -> Syn HTML a
input = flip (el "input") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
label :: [Props a] -> [Syn HTML a] -> Syn HTML a
label = el "label"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a :: [Props a] -> [Syn HTML a] -> Syn HTML a
a = el "a"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
mark :: [Props a] -> [Syn HTML a] -> Syn HTML a
mark = el "mark"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
ruby :: [Props a] -> [Syn HTML a] -> Syn HTML a
ruby = el "ruby"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rt :: [Props a] -> [Syn HTML a] -> Syn HTML a
rt = el "rt"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rp :: [Props a] -> [Syn HTML a] -> Syn HTML a
rp = el "rp"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdi :: [Props a] -> [Syn HTML a] -> Syn HTML a
bdi = el "bdi"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdo :: [Props a] -> [Syn HTML a] -> Syn HTML a
bdo = el "bdo"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbr :: [Props a] -> Syn HTML a
wbr = flip (el "wbr") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
details :: [Props a] -> [Syn HTML a] -> Syn HTML a
details = el "details"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summary :: [Props a] -> [Syn HTML a] -> Syn HTML a
summary = el "summary"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitem :: [Props a] -> [Syn HTML a] -> Syn HTML a
menuitem = el "menuitem"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menu :: [Props a] -> [Syn HTML a] -> Syn HTML a
menu = el "menu"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldset :: [Props a] -> [Syn HTML a] -> Syn HTML a
fieldset = el "fieldset"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legend :: [Props a] -> [Syn HTML a] -> Syn HTML a
legend = el "legend"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalist :: [Props a] -> [Syn HTML a] -> Syn HTML a
datalist = el "datalist"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroup :: [Props a] -> [Syn HTML a] -> Syn HTML a
optgroup = el "optgroup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygen :: [Props a] -> [Syn HTML a] -> Syn HTML a
keygen = el "keygen"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
output :: [Props a] -> [Syn HTML a] -> Syn HTML a
output = el "output"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progress :: [Props a] -> [Syn HTML a] -> Syn HTML a
progress = el "progress"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meter :: [Props a] -> [Syn HTML a] -> Syn HTML a
meter = el "meter"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
center :: [Props a] -> [Syn HTML a] -> Syn HTML a
center = el "center"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audio :: [Props a] -> [Syn HTML a] -> Syn HTML a
audio = el "audio"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
video :: [Props a] -> [Syn HTML a] -> Syn HTML a
video = el "video"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
source :: [Props a] -> Syn HTML a
source = flip (el "source") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
track :: [Props a] -> Syn HTML a
track = flip (el "track") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embed :: [Props a] -> Syn HTML a
embed = flip (el "embed") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
object :: [Props a] -> [Syn HTML a] -> Syn HTML a
object = el "object"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
param :: [Props a] -> Syn HTML a
param = flip (el "param") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
ins :: [Props a] -> [Syn HTML a] -> Syn HTML a
ins = el "ins"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
del :: [Props a] -> [Syn HTML a] -> Syn HTML a
del = el "del"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
small :: [Props a] -> [Syn HTML a] -> Syn HTML a
small = el "small"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
cite :: [Props a] -> [Syn HTML a] -> Syn HTML a
cite = el "cite"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfn :: [Props a] -> [Syn HTML a] -> Syn HTML a
dfn = el "dfn"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbr :: [Props a] -> [Syn HTML a] -> Syn HTML a
abbr = el "abbr"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
time :: [Props a] -> [Syn HTML a] -> Syn HTML a
time = el "time"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
var :: [Props a] -> [Syn HTML a] -> Syn HTML a
var = el "var"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
samp :: [Props a] -> [Syn HTML a] -> Syn HTML a
samp = el "samp"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbd :: [Props a] -> [Syn HTML a] -> Syn HTML a
kbd = el "kbd"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
caption :: [Props a] -> [Syn HTML a] -> Syn HTML a
caption = el "caption"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroup :: [Props a] -> [Syn HTML a] -> Syn HTML a
colgroup = el "colgroup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
col :: [Props a] -> Syn HTML a
col = flip (el "col") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
nav :: [Props a] -> [Syn HTML a] -> Syn HTML a
nav = el "nav"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
article :: [Props a] -> [Syn HTML a] -> Syn HTML a
article = el "article"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
aside :: [Props a] -> [Syn HTML a] -> Syn HTML a
aside = el "aside"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
address :: [Props a] -> [Syn HTML a] -> Syn HTML a
address = el "address"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
main_ :: [Props a] -> [Syn HTML a] -> Syn HTML a
main_ = el "main"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
body :: [Props a] -> [Syn HTML a] -> Syn HTML a
body = el "body"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figure :: [Props a] -> [Syn HTML a] -> Syn HTML a
figure = el "figure"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaption :: [Props a] -> [Syn HTML a] -> Syn HTML a
figcaption = el "figcaption"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dl :: [Props a] -> [Syn HTML a] -> Syn HTML a
dl = el "dl"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dt :: [Props a] -> [Syn HTML a] -> Syn HTML a
dt = el "dt"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
dd :: [Props a] -> [Syn HTML a] -> Syn HTML a
dd = el "dd"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
img :: [Props a] -> Syn HTML a
img = flip (el "img") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframe :: [Props a] -> [Syn HTML a] -> Syn HTML a
iframe = el "iframe"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvas :: [Props a] -> [Syn HTML a] -> Syn HTML a
canvas = el "canvas"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
math :: [Props a] -> [Syn HTML a] -> Syn HTML a
math = el "math"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
select :: [Props a] -> [Syn HTML a] -> Syn HTML a
select = el "select"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
option :: [Props a] -> [Syn HTML a] -> Syn HTML a
option = el "option"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textarea :: [Props a] -> [Syn HTML a] -> Syn HTML a
textarea = el "textarea"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
sub :: [Props a] -> [Syn HTML a] -> Syn HTML a
sub = el "sub"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
sup :: [Props a] -> [Syn HTML a] -> Syn HTML a
sup = el "sup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
br :: [Props a] -> Syn HTML a
br = flip (el "br") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
ol :: [Props a] -> [Syn HTML a] -> Syn HTML a
ol = el "ol"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquote :: [Props a] -> [Syn HTML a] -> Syn HTML a
blockquote = el "blockquote"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
code :: [Props a] -> [Syn HTML a] -> Syn HTML a
code = el "code"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
em :: [Props a] -> [Syn HTML a] -> Syn HTML a
em = el "em"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
i :: [Props a] -> [Syn HTML a] -> Syn HTML a
i = el "i"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
b :: [Props a] -> [Syn HTML a] -> Syn HTML a
b = el "b"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
u :: [Props a] -> [Syn HTML a] -> Syn HTML a
u = el "u"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
q :: [Props a] -> [Syn HTML a] -> Syn HTML a
q = el "q"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
script :: [Props a] -> [Syn HTML a] -> Syn HTML a
script = el "script"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
link :: [Props a] -> Syn HTML a
link = flip (el "link") []
