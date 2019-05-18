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
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Map.Strict (toList)

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

withWebSocketDataSource :: forall t m req x a.
  ( HasJSContext m
  , MonadFix m
  , MonadJSM (Performable m)
  , ToJSON (req x)
  , FromJSON (req x)
  , ToJSON x
  , FromJSON x
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
withWebSocketDataSource url _eClose _reconnect w = mdo
  (val, eRequest) <- runRequesterT w eResponse
  (eMapRawRequest, eResponse) <- matchResponsesWithRequests decodeRes eRequest (fmapMaybe decodeTag (_webSocket_recv ws))
  ws <- jsonWebSocket url $ def & webSocketConfig_send .~ (toList <$> eMapRawRequest) :: m (RawWebSocket t (Maybe (Int, x)))
  return val

  where

    decodeTag :: Value -> Maybe (Int, Value)
    decodeTag = undefined
    -- decodeTag bs =
    --   case decodeStrict bs of
    --     Nothing         -> Nothing :: Maybe (Int, Value)
    --     Just (tag, val) -> Just (tag, val)

    decodeRes :: (forall b. (ToJSON (req b), ToJSON b) => req b -> (Value, Value -> Identity b))
    decodeRes = undefined

  -- let
  --   eSend :: Event t [Maybe Value]
  --   eSend = (fmap . fmap) (\(_reqKey :=> req) -> Just $ toJSON req) (requesterDataToList <$> eRequest)
  -- (val, eRequest) <- runRequesterT w eResponse
  -- (_eRawRequest, eResponse) <- matchResponsesWithRequests undefined eRequest (decodeRequesterRes <$> _webSocket_recv ws)
  -- ws <- jsonWebSocket url $ WebSocketConfig eSend eClose reconnect []
  -- return val

    -- decodeRequesterRes :: Maybe Value -> (Int, Maybe Value)
    -- decodeRequesterRes mValue = undefined

  --   decodeValue :: (FromJSON (req x), FromJSON x) => Maybe Value -> Identity x
  --   decodeValue Nothing = error "error"
  --   decodeValue (Just v) = 
  --     case fromJSON v of
  --       Error s -> error s
  --       Success a -> a

  --   decoder :: forall req x. (FromJSON x) => (req x) -> (Maybe Value, Maybe Value -> Identity x)
  --   decoder req = (Just $ toJSON req, decodeValue)

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
      _ <- widgetHold (text "Waiting for Response1") ((\b -> text ("Length is: " <> (pack . show . not $ b))) <$> eResp)
      blank

deriveJSONGADT ''RequestG