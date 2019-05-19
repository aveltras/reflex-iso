{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

-- import Data.Aeson
-- import Data.Functor.Identity (Identity(..))
import Reflex.Dom hiding (Value)
-- import Haskus.Utils.Variant
-- import Haskus.Utils.ContFlow

import Common

main :: IO ()
main = do
  mainWidget $ withWebSocketDataSource "ws://localhost:8080" never True $ htmlW False



-- printU :: V '[String,Int,Float] -> IO ()
-- printU v = (variantToCont v) >::>
--    ( \s -> putStrLn ("Found string: " ++ (s :: String))
--    , \i -> putStrLn ("Found int: " ++ show (i :: Int))
--    , \f -> putStrLn ("Found float: " ++ show (f :: Float))
--    )