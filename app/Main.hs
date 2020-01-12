{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad (void)

import Control.Monad (forever)

import qualified Data.Map as M
import qualified Data.Text as T

import qualified Connector.WebSocket as WS

import qualified Connector.Log as Log
import qualified Connector.HTTP as HTTP

import           Replica.VDOM             (Attr(AText, ABool, AEvent, AMap), HTML, DOMEvent, VDOM(VNode, VText), defaultIndex, fireEvent)
import           Replica.VDOM.Types       (DOMEvent(DOMEvent))
import           Replica.DOM
import           Replica.Props hiding (async)
import           Replica.Events
import           Syn

import Network.HTTP.Types.Status
import Network.WebSockets.Connection
import Network.Wai

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.Replica as Replica

import Prelude hiding (div)

-- testConcur :: IO ()
-- testConcur = Log.logger $ \log -> do
--   v1 <- registerDelay 1000000
--   v2 <- registerDelay 2000000
--   v3 <- registerDelay 1500000
--   v4 <- registerDelay 3000000
--   v5 <- registerDelay 2500000
-- 
--   (_, rs) <- runConcur $ do
--     (_, rs) <- orr'
--       [ dp log v3 "V3"
--       , dp log v5 "V5"
--       , do
--           (_, rs) <- orr' [ dp log v1 "A", dp log v2 "B", dp log v4 "C" ]
--           (_, rs) <- orr' rs
--           (_, rs) <- orr' rs
--           pure ()
--       ]
--     (_, rs) <- orr' rs
--     orr' rs
-- 
--   print $ length rs
-- 
--   where
--     dp log v s = do
--       log ("BEFORE: " <> s)
--       step $ do
--         v' <- readTVar v
--         check v'
--       log ("AFTER: " <> s)
-- 
--     f c n = do
--       step $ writeTChan c (show n)
--       f c (n + 1)

-- testConnectors :: IO ()
-- testConnectors = do
--   HTTP.http 3921 $ \http ->
--     WS.websocket 3922 defaultConnectionOptions $ \wss ->
--     WS.websocket 3923 defaultConnectionOptions $ \wss2 ->
--     Log.logger $ \log -> do
-- 
--       runConcur $ auth http log wss wss2
-- 
--     where
--       auth http log wss wss2 = do
--         r <- HTTP.receive http $ \req respond -> do
--           r <- respond $ responseLBS status200 [] "Hello World"
--           pure (r, "good")
--         log r
--         server log wss wss2
--       
--       server log wss wss2 = withPool $ \pool -> forever $ do
--         [ws, ws2] <- andd [ WS.accept wss, WS.accept wss2 ]
--         spawn pool (go log ws ws2)
--         
--       go log ws ws2 = do
--         r <- orr
--           [ fmap Left  <$> WS.receive ws
--           , fmap Right <$> WS.receive ws2
--           ]
--         case r of
--           Nothing  -> pure ()
--           _  -> do
--             log $ show r
--             go log ws ws2

testWebsockets :: IO (Context () ())
testWebsockets =
  WS.websocket 3922 defaultConnectionOptions $ \wss -> do
  WS.websocket 3923 defaultConnectionOptions $ \wss2 -> run $ do
    (ws, ws2) <- andd (WS.accept wss, WS.accept wss2)

    d <- WS.receive ws
    d2 <- WS.receive ws2

    WS.send ws d
    WS.send ws d2

    WS.send ws2 d
    WS.send ws2 d2

testChat :: IO (Context () ())
testChat
  = WS.websocket 3922 defaultConnectionOptions $ \wss ->
    run $
    pool $ \p ->
    local $ \msg -> do
      acceptConn p wss msg
  where
    acceptConn p wss msg = do
      ws <- WS.accept wss
      spawn p (chatConn ws msg)
      acceptConn p wss msg

    chatConn ws msg = do
      r <- orr [ Left <$> WS.receive ws, Right <$> await msg ]
      case r of
        Left m -> do
          emit msg m
          WS.send ws m
        Right msg -> WS.send ws msg
      chatConn ws msg

main :: IO ()
main = pure ()

-- Replica ---------------------------------------------------------------------

runReplica p = do
  ctx   <- newMVar (Just (0, p, E))
  block <- newMVar ()
  Warp.run 3985 $ Replica.app (defaultIndex "Synchron" []) defaultConnectionOptions Prelude.id () $ \() -> do
    takeMVar block
    modifyMVar ctx $ \ctx' -> case ctx' of
      Just (eid, p, v) -> do
        r <- stepAll mempty eid p v
        case r of
          (Left _, v', _) -> do
            -- print (show (runHTML (foldV v') (Context ctx)))
            pure (Nothing, Just (runHTML (foldV v') (Context ctx), (), \_ -> pure (pure ())))
          (Right (eid', p'), v', _) -> do
            let html = runHTML (foldV v') (Context ctx)
            pure
              ( Just (eid', p', v')
              , Just (html, (), \re -> fmap (>> putMVar block()) $ fireEvent html (Replica.evtPath re) (Replica.evtType re) (DOMEvent $ Replica.evtEvent re))
              )
      Nothing -> pure (Nothing, Nothing)

data ContainerProps = Click (Event Internal DOMEvent)
data Container = Label T.Text | Number Int | Container [ContainerProps] [Container]

toHTML (Label x) = HTML $ \_ -> [VText x]
toHTML (Number x) = HTML $ \_ -> [VText (T.pack $ show x)]
toHTML (Container props children) = HTML $ \ctx ->
  [ VNode "div"
      (M.fromList $ fmap (toProps ctx) props)
      (concatMap (($ ctx) . runHTML . toHTML) children)
  ]
  where
    toProps ctx (Click e) = ("onClick", AEvent $ \de -> void $ push ctx e de)

-- abstractConter :: Event Internal Int -> Int -> Syn Container a
-- abstractConter o x = local $ \e -> local $ \f -> do
--   view (Container [Click e] [Number x])
--   await e
--   view (Container [Click f] [Label "You clicked!"])
--   await f
--   emit o (x + 1)
--   abstractConter o (x + 1)
-- 
-- realCounter x = local $ \y -> do
--   void $ andd (mapView toHTML (abstractConter y x), label x y)
--   where
--     label x y = do
--       Right x' <- orr [ Left <$> text (T.pack $ show x), Right <$> await y ]
--       label x' y

counter x = do
  div [ onClick ] [ text (T.pack $ show x) ]
  counter (x + 1)

testReplica = do
  runReplica $ local $ \e -> do
    div [ style [("color", "red")], onClick ] [ text "Synchron" ]
    div [ style [("color", "green")], onClick ] [ text "Synchron2" ]
    div [ style [("color", "blue")] ] [ text "Synchron3" ]

inputOnEnter v = do
  e <- input [ autofocus True, placeholder "Enter", value v, Left <$> onInput, Right <$> onKeyDown ]
  case e of
    Left e  -> inputOnEnter (targetValue $ target e)
    Right e -> if kbdKey e == "Enter"
      then pure v
      else inputOnEnter v

addition = do
  a <- inputOnEnter ""
  button [ onClick ] [ text "Next" ]
  b <- inputOnEnter ""
  button [ onClick ] [ text "Add" ]
  div [ onClick ] [ text ("Result :" <> T.pack (show (read (T.unpack a) + read (T.unpack b)))) ]
  addition

additions2 = andd (addition, addition)

additions = pool $ \p -> do
  spawn p addition
  spawn p addition
  spawn p addition
  spawn p addition
  Syn.forever
  
--------------------------------------------------------------------------------

shared :: Syn () ()
shared = local $ \end -> local $ \st -> pool $ \p -> do
  spawn p (set st end)
  spawn p (get st)
  await end
  where
    set st end = do
      emit st 4
      emit st 5
      emit st 6
      emit st 7
      emit end ()

    get st = do
      a <- await st
      async (print a)
      get st
