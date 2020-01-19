{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}

module Syn.Replica.DOM where

import           Control.Concurrent       (MVar, newMVar, newEmptyMVar, modifyMVar, putMVar, takeMVar)
import           Control.Monad            (void)
import           Control.Monad.Trans.Reader (runReaderT)

import           Replica.Props            (Props(Props), Prop(PropText, PropBool, PropEvent, PropMap), key)

import qualified Data.Aeson               as A
import           Data.Bifunctor           (second)
import           Data.Monoid              ((<>))
import qualified Data.Text                as T

import qualified Data.Map                 as M

import           Replica.VDOM             (Attr(AText, ABool, AEvent, AMap), DOMEvent, VDOM(..))
import qualified Replica.VDOM             as R

import           Syn.View
import qualified Syn

import Unsafe.Coerce (unsafeCoerce)
import Debug.Trace

data Context a = Context
  Syn.NodeId
  (Syn.Event Syn.Internal [ViewPatch HTML])
  (MVar (Maybe (Int, VSyn HTML a, R.HTML)))

newtype HTML = HTML { runHTML :: Context () -> R.HTML }

newContext :: Syn.NodeId -> VSyn HTML a -> IO (Context a)
newContext nid p = do
  ve <- Syn.newEvent' (<>) nid
  Context <$> pure nid <*> pure ve <*> newMVar (Just (0, p, []))

patch :: ViewPatch R.HTML -> R.HTML -> R.HTML 
-- patch a b
--   | trace ("A" <> show (A.encode a) <> "===" <> show (A.encode b)) False = undefined
patch ([], v) _ = error "patch"
patch ([p], v) h
  | p >= length h = h <> take (p - length h) (repeat $ VText "") <> v
  | otherwise = take p h <> v <> drop (p + 1) h
patch ((p:ps), v) h = take p h <> [patchChildren ps v (h !! p)] <> drop (p + 1) h
  where
    patchChildren ps v n@(VNode e attrs children)
      -- = trace ("TRACE: " <> show (A.encode n)) $ VNode e attrs (patch (ps, v) children)
      = VNode e attrs (patch (ps, v) children)
    patchChildren _ _ n = error (show $ A.encode n)

push :: Context () -> [Syn.EventValue] -> IO (Maybe ())
push ctx@(Context nid e v) es = do
  modifyMVar v $ \v -> case v of
    Just (eid, p, v) -> do
      (r, eid', _) <- Syn.stepAll' m nid eid (runView e p) Syn.E
  
      case r of
        Left (Just (Left a)) -> pure (Nothing, Just a)
        Left (Just (Right (vp, next))) -> do
          let vp' = fmap (second (flip runHTML ctx)) (vp)

          traceIO $ show $ "VP: " <> A.encode vp'
          -- traceIO $ show $ "V BEFORE: "<> A.encode v
  
          pure (Just (eid', next, foldr (\(path, v) h -> patch (reverse path, v) h) v vp'), Nothing)
        Left Nothing -> pure (Nothing, Nothing)
        Right p' -> pure (Just (eid', left <$> liftSyn p', v), Nothing)
  
    _ -> pure (Nothing, Nothing)
  where
    m = M.fromList
      [ (ei, (Syn.EventValue (Syn.Event conc ei) a))
      | Syn.EventValue (Syn.Event conc ei) a <- es
      ]

    left (Left a) = a

el :: T.Text -> [Props a] -> [VSyn HTML a] -> VSyn HTML a
el e attrs children = do
  attrs' <- traverse toAttr attrs
  view (HTML $ \ctx -> [VNode e (M.fromList $ fmap (second ($ ctx) . fst) attrs') []]) (children <> concatMap snd attrs')
  where
    toAttr :: Props a -> VSyn HTML ((T.Text, Context () -> Attr), [VSyn HTML a])
    toAttr (Props k (PropText v)) = pure ((k, \_ -> AText v), [])
    toAttr (Props k (PropBool v)) = pure ((k, \_ -> ABool v), [])
    toAttr (Props k (PropEvent extract)) = do
      e <- local
      pure ((k, \ctx -> AEvent $ \de -> void $ push ctx [Syn.EventValue e de]), [extract <$> await e])
    toAttr (Props k (PropMap m)) = do
      m' <- mapM toAttr m
      pure ((k, \ctx -> AMap $ M.fromList $ fmap (second ($ ctx) . fst) m'), concatMap snd m')

text :: T.Text -> VSyn HTML a
text txt = view (HTML $ \_ -> [VText txt]) []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
div :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
div  = el "div"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
table :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
table  = el "table"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
thead :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
thead  = el "thead"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbody :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
tbody  = el "tbody"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
tr :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
tr  = el "tr"

-- | Contains `Key`, inteded to be used for child replacement patch
--
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr>

trKeyed :: T.Text -> [Props a] -> [VSyn HTML a] -> VSyn HTML a
trKeyed k props = el "tr" (key k:props)

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
th :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
th  = el "th"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
td :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
td  = el "td"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfoot :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
tfoot  = el "tfoot"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
section :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
section  = el "section"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
header :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
header  = el "header"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footer :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
footer  = el "footer"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
button :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
button = el "button"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
form :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
form = el "form"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
p :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
p = el "p"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
s :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
s = el "s"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ul :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
ul = el "ul"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
span :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
span = el "span"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strong :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
strong = el "strong"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
li :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
li = el "li"

-- | Contains `Key`, inteded to be used for child replacement patch
--
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li>
--
liKeyed :: T.Text -> [Props a] -> [VSyn HTML a] -> VSyn HTML a
liKeyed k props = el "li" (key k:props)

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1 :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
h1 = el "h1"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2 :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
h2 = el "h2"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3 :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
h3 = el "h3"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4 :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
h4 = el "h4"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5 :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
h5 = el "h5"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6 :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
h6 = el "h6"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hr :: [Props a] -> VSyn HTML a
hr = flip (el "hr") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
pre :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
pre = el "pre"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
input :: [Props a] -> VSyn HTML a
input = flip (el "input") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
label :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
label = el "label"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
a = el "a"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
mark :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
mark = el "mark"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
ruby :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
ruby = el "ruby"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rt :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
rt = el "rt"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rp :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
rp = el "rp"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdi :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
bdi = el "bdi"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdo :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
bdo = el "bdo"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbr :: [Props a] -> VSyn HTML a
wbr = flip (el "wbr") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
details :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
details = el "details"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summary :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
summary = el "summary"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitem :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
menuitem = el "menuitem"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menu :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
menu = el "menu"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldset :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
fieldset = el "fieldset"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legend :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
legend = el "legend"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalist :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
datalist = el "datalist"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroup :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
optgroup = el "optgroup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygen :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
keygen = el "keygen"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
output :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
output = el "output"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progress :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
progress = el "progress"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meter :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
meter = el "meter"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
center :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
center = el "center"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audio :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
audio = el "audio"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
video :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
video = el "video"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
source :: [Props a] -> VSyn HTML a
source = flip (el "source") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
track :: [Props a] -> VSyn HTML a
track = flip (el "track") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embed :: [Props a] -> VSyn HTML a
embed = flip (el "embed") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
object :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
object = el "object"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
param :: [Props a] -> VSyn HTML a
param = flip (el "param") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
ins :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
ins = el "ins"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
del :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
del = el "del"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
small :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
small = el "small"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
cite :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
cite = el "cite"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfn :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
dfn = el "dfn"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbr :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
abbr = el "abbr"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
time :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
time = el "time"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
var :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
var = el "var"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
samp :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
samp = el "samp"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbd :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
kbd = el "kbd"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
caption :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
caption = el "caption"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroup :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
colgroup = el "colgroup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
col :: [Props a] -> VSyn HTML a
col = flip (el "col") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
nav :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
nav = el "nav"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
article :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
article = el "article"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
aside :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
aside = el "aside"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
address :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
address = el "address"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
main_ :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
main_ = el "main"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
body :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
body = el "body"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figure :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
figure = el "figure"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaption :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
figcaption = el "figcaption"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dl :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
dl = el "dl"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dt :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
dt = el "dt"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
dd :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
dd = el "dd"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
img :: [Props a] -> VSyn HTML a
img = flip (el "img") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframe :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
iframe = el "iframe"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvas :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
canvas = el "canvas"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
math :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
math = el "math"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
select :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
select = el "select"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
option :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
option = el "option"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textarea :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
textarea = el "textarea"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
sub :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
sub = el "sub"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
sup :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
sup = el "sup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
br :: [Props a] -> VSyn HTML a
br = flip (el "br") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
ol :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
ol = el "ol"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquote :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
blockquote = el "blockquote"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
code :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
code = el "code"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
em :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
em = el "em"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
i :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
i = el "i"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
b :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
b = el "b"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
u :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
u = el "u"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
q :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
q = el "q"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
script :: [Props a] -> [VSyn HTML a] -> VSyn HTML a
script = el "script"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
link :: [Props a] -> VSyn HTML a
link = flip (el "link") []
