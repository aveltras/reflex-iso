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
import qualified Data.ByteString as BS
import Data.Functor.Identity (Identity(..))
import Language.Javascript.JSaddle.WebSockets
import Network.HTTP.Types
import Network.WebSockets (acceptRequest, ServerApp, sendTextData)
import Network.Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)
import Reflex.Dom hiding (Value)
import Reflex.Dom.Builder.Static (renderStatic)
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wai.Handler.Warp as W
import Data.Text (Text)

main :: IO ()
main = do
  putStrLn "server"
  -- W.run 8080 =<< jsaddleOr defaultConnectionOptions (mainWidget' $ withWebSocketDataSourceG "http://localhost:8080" never True $ htmlW False) (websocketsOr defaultConnectionOptions wsApp app)
  W.run 8080 =<< jsaddleOr defaultConnectionOptions (mainWidget' $ el "h1" $ text "tac") (websocketsOr defaultConnectionOptions wsApp app)

wsApp :: ServerApp
wsApp pending_conn = do
    conn <- acceptRequest pending_conn
    putStrLn "test"
    sendTextData conn ("Hello, client!" :: Text)

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
  RequestG2 int -> return $ Identity "test"

renderFrontend ::
  ( t ~ DomTimeline
  , w ~ PostBuildT t (StaticDomBuilderT t (PerformEventT t DomHost))
  ) => w () -> IO BS.ByteString
renderFrontend w = do 
  (_, bs) <- renderStatic w 
  return bs
