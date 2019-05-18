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
  mainWidget $ withWebSocketDataSource "http://localhost:8080" never True decodeRes $ htmlW False


