{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Server where

import Common

import qualified Data.ByteString as BS
import Data.Functor.Identity (Identity(..))
import Language.Javascript.JSaddle.WebSockets
import Network.HTTP.Types
import Network.WebSockets (acceptRequest, ServerApp, sendBinaryData, receiveData)
import Network.Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)
import Reflex.Dom hiding (Value)
import Reflex.Dom.Builder.Static (renderStatic)
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wai.Handler.Warp as W
import Data.Text (Text)
import Control.Concurrent.Async (concurrently_)

main :: IO ()
main = do
  putStrLn "server"
  concurrently_ jsaddle wsServer
  where
    jsaddle = debugOr 8081 (mainWidget' $ withWebSocketDataSource "http://localhost:8080" never True decodeRes $ htmlW False) app
    wsServer = W.run 8080 (websocketsOr defaultConnectionOptions wsApp backupApp)

backupApp :: Application
backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

wsApp :: ServerApp
wsApp pending_conn = do
    conn <- acceptRequest pending_conn

    print "connection to WS"
    bsReq <- receiveData conn :: IO BS.ByteString
    print "received bytestring request"
    case decodeTag bsReq of
      Just (int, val) -> print val
      Nothing -> error "error decoding request"
    return ()
    -- sendBinaryData conn ("Hello, client!" :: Text)

app :: Application
app req respond = do
  case (requestMethod req, pathInfo req) of
    ("GET", ["jsaddle.js"]) -> respond $ responseLBS status200 [("Content-Type", "application/javascript")] (jsaddleJs False)
    _ -> do
      bs <- renderFrontend $ withLocalDataSource handler $ htmlW True
      -- bs <- renderFrontend staticW
      respond $ responseLBS
        status200
        [("Content-Type", "text/html")]
        (LBS.fromStrict bs)


handler :: RequestG a -> IO (Identity a)
handler = \case
  RequestG1 -> return $ Identity True
  RequestG2 _int -> return $ Identity "test"

renderFrontend ::
  ( t ~ DomTimeline
  , w ~ PostBuildT t (StaticDomBuilderT t (PerformEventT t DomHost))
  ) => w () -> IO BS.ByteString
renderFrontend w = do 
  (_, bs) <- renderStatic w 
  return bs
