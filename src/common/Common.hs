{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Common where

import Data.Dependent.Sum(DSum(..), (==>))
import Data.Aeson.GADT.TH
import Data.Functor.Identity (Identity(..))
import Data.Proxy (Proxy(..))
import Data.Aeson
import Control.Applicative (Const)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix)
import GHCJS.DOM.Types (MonadJSM)
import Reflex.Dom hiding (Error, HNil, Value)
import Data.Text (Text, pack)

main :: String
main = "common"

type HasDataSource t req m = (Requester t m, Request m ~ req, Response m ~ Identity)
type WithDataSource t req m = RequesterT t req Identity m

data RequestG a where
  RequestG1 :: RequestG Bool
  RequestG2 :: Int -> RequestG Text

withLocalDataSource ::
  ( MonadFix m
  , PerformEvent t m
  , MonadIO (Performable m)
  ) => (forall x. (req x) -> IO (Identity x))
  -> WithDataSource t req m a
  -> m a
withLocalDataSource h w = mdo
  (val, eRequest) <- runRequesterT w eResponse
  eResponse <- performEvent $ liftIO . (traverseRequesterData h) <$> eRequest
  return val

withWebSocketDataSource ::
  ( HasJSContext m
  , MonadFix m
  , MonadJSM (Performable m)
  -- , ToJSON req
  -- , FromJSON req
  , MonadJSM m
  , MonadHold t m
  , PerformEvent t m 
  , PostBuild t m
  , TriggerEvent t m )
  => Text -- WebSocket URL
  -> Event t (Word, Text) -- close event
  -> Bool -- reconnect on close
  -> WithDataSource t req m a -- widget
  -> m a
withWebSocketDataSource url eClose reconnect w = mdo
  let
    eSend :: Event t [Maybe Value]
    eSend = (fmap . fmap) (\(_reqKey :=> req) -> Just $ toJSON req) (requesterDataToList <$> eRequest)
  (val, eRequest) <- runRequesterT w eResponse
  (_eRawRequest, eResponse) <- matchResponsesWithRequests decoder eRequest (decodeValue <$> _webSocket_recv ws)
  ws <- jsonWebSocket url $ WebSocketConfig eSend eClose reconnect []
  return val
  where
    decodeValue :: (FromJSON r) => Maybe Value -> r
    decodeValue Nothing = error "error"
    decodeValue (Just v) = 
      case fromJSON v of
        Error s -> error s
        Success a -> a

    decoder :: forall req x. (FromJSON x) => (req x) -> (Maybe Value, Maybe Value -> Identity x)
    decoder req = (Just $ toJSON req, decodeValue)

getResponse
  :: (HasDataSource t req m)
  => Event t (req x) -> m (Event t x)
getResponse req = do
  resp <- requesting req
  return $ (\(Identity b) -> b) <$> resp

htmlW ::
  ( DomBuilder t m
  , MonadHold t m
  , HasDataSource t RequestG m
  , PostBuild t m
  ) => Bool -> m ()
htmlW b = do
  ePb <- getPostBuild
  el "html" $ do
    el "title" $ text "blabla title2"
    if b
      then elAttr "script" ("src" =: "jsaddle.js") $ blank
      else blank
    el "body" $ do
      el "div" $ text "body"
      eResp <- getResponse ((RequestG1) <$ ePb)
      _ <- widgetHold (text "Waiting for Response1") ((\b -> text ("Length is: " <> (pack . show $ b))) <$> eResp)
      blank

deriveJSONGADT ''RequestG