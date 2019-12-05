{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Connector.WebSocket as WS

import qualified Connector.Log as Log
import qualified Connector.HTTP as HTTP

import Concur

import Network.HTTP.Types.Status
import Network.WebSockets.Connection
import Network.Wai

test :: IO ()
test = do
  HTTP.http 3921 $ \http -> do
  WS.websocket 3922 defaultConnectionOptions $ \wss -> do
  WS.websocket 3923 defaultConnectionOptions $ \wss2 -> do
  Log.logger $ \log -> do

    runConcur $ auth http log wss wss2

    where
      auth http log wss wss2 = do
        r <- HTTP.receive http $ \req respond -> do
          r <- respond $ responseLBS status200 [] "Hello World"
          pure (r, "good")
        log r
        server' log wss wss2
      
      server' log wss wss2
        = loopOrr [ Left <$> andd [ WS.accept wss, WS.accept wss2 ] ] $ \r -> do
            case r of
              Left [ws, ws2] -> pure
                [ Left  <$> andd [ WS.accept wss, WS.accept wss2 ]  -- restart accept
                , Right <$> go log ws ws2                     -- add new connection
                ]
              Right _ -> pure []

      server log wss wss2 conns = do
        log ("LENGTH: " <> show (length conns))
        (r, ks) <- orr conns
        case r of
          Left [ws, ws2] -> server log wss wss2 $ concat
            [ [ Left  <$> andd [ WS.accept wss, WS.accept wss2 ] ]  -- restart accept
            , [ Right <$> go log ws ws2 ]                     -- add new connection
            , ks                                              -- keep rest of connections
            ]
          Right _        -> server log wss wss2 ks
        
        
      go log ws ws2 = do
        (r, _) <- orr
          [ fmap Left  <$> WS.receive ws
          , fmap Right <$> WS.receive ws2
          ]
        case r of
          Nothing  -> pure ()
          _  -> do
            log $ show r
            go log ws ws2

main :: IO ()
main = print ()
