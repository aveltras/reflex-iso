{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Client where

import Data.Aeson
import Data.Functor.Identity (Identity(..))
import Reflex.Dom hiding (Value)

import Common

main :: IO ()
main = do
  mainWidget $ withWebSocketDataSource "http://localhost:8080" never True decodeRes htmlW

decodeRes :: RequestG a -> (Value, Value -> Identity a)
decodeRes = \case
  req@RequestG1 -> (toJSON req, const (Identity True))
  req@(RequestG2 _int) -> (toJSON req, const (Identity "text"))

