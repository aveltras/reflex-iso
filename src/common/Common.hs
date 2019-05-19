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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Common where

import Data.Aeson.GADT.TH
import Data.Functor.Identity (Identity(..))
import Data.Aeson
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix)
import Data.Proxy (Proxy(..))
import Data.Dependent.Map (DMap, Some(..))
import GHCJS.DOM.Types (MonadJSM)
import Reflex.Dom hiding (Error, HNil, Value)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Map.Strict (toList)
import Data.Constraint.Forall
import Data.Constraint.Extras
import Data.Constraint.Extras.TH
import Data.Constraint
import Haskus.Utils.Variant
import Haskus.Utils.ContFlow
-- import Data.Vinyl.CoRec

data Rq1 = Rq1 deriving (Generic, FromJSON, ToJSON)
-- data Rs1 = Rs1 Bool deriving (Generic, FromJSON, ToJSON)
instance Message Rq1 where
  type ResponseT Rq1 = Text

data Rq2 = Rq2 Text deriving (Generic, FromJSON, ToJSON)
-- data Rs2 = Rs2 (Int, Bool) deriving (Generic, FromJSON, ToJSON)
instance Message Rq2 where
  type ResponseT Rq2 = (Int, Bool)


-- f :: MyReq -> IO ()
-- f r = variantToCont r >::>
--    ( \Rq1 -> putStrLn "Found string: "
--    , \(Rq2 t) -> putStrLn "Found int: "
--    )

class ( FromJSON req
      , ToJSON req
      , FromJSON (ResponseT req)
      , ToJSON (ResponseT req)
      ) => Message req where
  type ResponseT req

type MyReq = '[Rq1, Rq2]

data IsRequest sum req where
  IsRequest :: (Member req sum, Message req) => req -> IsRequest sum req

data IsResponse sum req where
  IsResponse :: (Member req sum, Message req) => ResponseT req -> IsResponse sum req

type HasDataSource req t m = (Requester t m, Request m ~ req, Response m ~ Identity)
type WithDataSource req t m = RequesterT t req Identity m

data RequestG :: * -> * where
  RequestG1 :: RequestG Bool
  RequestG2 :: Int -> RequestG Text

withLocalDataSource ::
  ( MonadFix m
  , PerformEvent t m
  , MonadIO (Performable m)
  ) => (forall x. (req x) -> IO (Identity x))
  -> WithDataSource req t m a
  -> m a
withLocalDataSource h w = mdo
  (val, eRequest) <- runRequesterT w eResponse
  eResponse <- performEvent $ liftIO . (traverseRequesterData h) <$> eRequest
  return val

withWebSocketDataSource :: forall t m req a.
  ( HasJSContext m
  , MonadFix m
  , MonadJSM (Performable m)
  , MonadJSM m
  , Has ToJSON req
  , Has FromJSON req
  , MonadHold t m
  , PerformEvent t m 
  , PostBuild t m
  , TriggerEvent t m )
  => Text -- WebSocket URL
  -> Event t (Word, Text) -- close event
  -> Bool -- reconnect on close
  -> WithDataSource req t m a -- widget
  -> m a
withWebSocketDataSource url _eClose _reconnect w = mdo
  let
    wsConfig = def & webSocketConfig_send .~ eSend
    eSend = (fmap . fmap) encodeReq (toList <$> eMapRawRequest) :: Event t [BS.ByteString]
  (val, eRequest) <- runRequesterT w eResponse
  (eMapRawRequest, eResponse) <- matchResponsesWithRequests decodeRes2 eRequest (fmapMaybe decodeTag (_webSocket_recv ws))
  ws <- webSocket url wsConfig
  return val

  where

    encodeReq :: (Int, Value) -> BS.ByteString
    encodeReq = LBS.toStrict . encode

    decodeRes2 :: forall b. req b -> (Value, Value -> Identity b)
    decodeRes2 reqG = (has @ToJSON reqG toJSON reqG, f)
      where
        f val = do
          let result = has @FromJSON reqG fromJSON val
          case result of
            Error _s -> error "boom"
            Success a -> Identity a

    -- decodeRes2 :: forall req. (IsRequest sum req) -> (Value, Value -> IsResponse sum req)
    -- decodeRes2 (IsRequest reqG) = (toJSON reqG, f)
    --   where
    --     f val = do
    --       let result = fromJSON val
    --       case result of
    --         Error _s -> error "boom"
    --         Success a -> IsResponse a

decodeTag :: BS.ByteString -> Maybe (Int, Value)
decodeTag bs =
  case decodeStrict bs of
    Nothing         -> Nothing :: Maybe (Int, Value)
    Just (val, rst) -> Just (val, rst)

getResponse
  :: (HasDataSource req t m)
  => Event t (req x) -> m (Event t x)
getResponse req = do
  resp <- requesting req
  return $ (\(Identity b) -> b) <$> resp

htmlW ::
  ( DomBuilder t m
  , MonadHold t m
  , HasDataSource RequestG t m
  , PostBuild t m
  ) => Bool -> m ()
htmlW b = do
  ePb <- getPostBuild
  el "html" $ do
    el "title" $ text "blabla title2"
    if b
      then elAttr "script" ("src" =: "jsaddle.js") $ blank
      -- else blank
      else elAttr "script" ("src" =: "all.js") $ blank
    el "body" $ do
      el "div" $ text "body"
      eResp <- getResponse ((RequestG1) <$ ePb)
      _ <- widgetHold (text "Waiting for Loading") ((\(b2) -> text ("Length (prerender) is: " <> (pack . show $ b2))) <$> eResp)
      eButton <- button "call websocket"
      eResp2 <- getResponse ((RequestG1) <$ eButton)
      _ <- widgetHold (text "Waiting for Websocket") ((\(b2) -> text ("Length (websocket) is: " <> (pack . show $ b2))) <$> eResp2)
      blank

-- instance ArgDict RequestG where
--   type ConstraintsFor RequestG c = (c Bool, c Text)
--   argDict = \case
--     RequestG1 -> Dict
--     RequestG2 _ -> Dict

deriveJSONGADT ''RequestG
deriveArgDict ''RequestG