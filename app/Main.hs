{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import qualified Connector.WebSocket as WS

import qualified Connector.Log as Log
import qualified Connector.HTTP as HTTP

import Concur

import Network.HTTP.Types.Status
import Network.WebSockets.Connection
import Network.Wai

testConnectors :: IO ()
testConnectors = do
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
        server'' log wss wss2
      
      server'' log wss wss2 = withPool $ \pool -> forever $ do
        [ws, ws2] <- andd [ WS.accept wss, WS.accept wss2 ]
        spawn pool (go log ws ws2)

      go log ws ws2 = do
        r <- orr
          [ fmap Left  <$> WS.receive ws
          , fmap Right <$> WS.receive ws2
          ]
        case r of
          Nothing  -> pure ()
          _  -> do
            log $ show r
            go log ws ws2

main :: IO ()
main = pure ()
