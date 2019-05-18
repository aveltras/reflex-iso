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

import Data.Aeson
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.Functor.Identity (Identity(..))
import Language.Javascript.JSaddle.WebSockets
import Network.HTTP.Types
import Network.WebSockets (acceptRequest, ServerApp, sendBinaryData, receiveData, forkPingThread)
import Network.Wai
import Control.Monad (forever)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)
import Reflex.Dom hiding (Error, Value)
import Reflex.Dom.Builder.Static (renderStatic)
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wai.Handler.Warp as W
import Data.Text (Text)
import Control.Concurrent.Async (concurrently_)
import Network.Wai.Middleware.Static

main :: IO ()
main = do
  putStrLn "server"
  let jSaddle = False
  if jSaddle
    then concurrently_ jsaddle wsServer
    else concurrently_ (W.run 8081 $ mainApp False) wsServer
  where
    jsaddle = debugOr 8081 (mainWidget' $ withWebSocketDataSource "ws://localhost:8080" never True decodeRes $ htmlW False) (mainApp False)
    mainApp b = staticPolicy (addBase "static") (app b)
    wsServer = W.run 8080 (websocketsOr defaultConnectionOptions wsApp backupApp)

backupApp :: Application
backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

wsApp :: ServerApp
wsApp pending_conn = do
    conn <- acceptRequest pending_conn
    forever $ do
      -- print "connection to WS"
      bsReq <- receiveData conn :: IO BS.ByteString
      forkPingThread conn 30

      -- print "received bytestring request"

      case decodeTag bsReq of
        Just (int, val) -> do
          let resp = encode (int, True)
          print $ "Response: " <> (show resp)
          sendBinaryData conn resp  -- ("Hello, client!" :: Text)
        _ -> error "boom"
          -- print val
          -- case fromJSON val of
          --   Error s -> error s
          --   Success req -> do
          --     case req of
          --       RequestG1 -> undefined
          --       RequestG2 _int -> undefined
          --     -- resp <- handler req
          --     return ()
        Nothing -> error "error decoding request"
      return ()
      -- sendBinaryData conn ("Hello, client!" :: Text)

app :: Bool -> Application
app b req respond = do
  case (requestMethod req, pathInfo req) of
    ("GET", ["jsaddle.js"]) -> respond $ responseLBS status200 [("Content-Type", "application/javascript")] (jsaddleJs False)
    _ -> do
      bs <- renderFrontend $ withLocalDataSource handler $ htmlW b
      respond $ responseLBS
        status200
        [("Content-Type", "text/html")]
        (LBS.fromStrict bs)


handler :: RequestG a -> IO (Identity a)
handler = \case
  RequestG1 -> return $ Identity False
  RequestG2 _int -> return $ Identity "test"

renderFrontend ::
  ( t ~ DomTimeline
  , w ~ PostBuildT t (StaticDomBuilderT t (PerformEventT t DomHost))
  ) => w () -> IO BS.ByteString
renderFrontend w = do 
  (_, bs) <- renderStatic w 
  return bs
