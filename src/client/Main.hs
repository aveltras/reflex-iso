{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Data.Aeson
-- import Data.Functor.Identity (Identity(..))
import Reflex.Dom hiding (Value)

import Common

main :: IO ()
main = do
  mainWidget $ withWebSocketDataSource "ws://localhost:8080" never True decodeRes $ htmlW False


