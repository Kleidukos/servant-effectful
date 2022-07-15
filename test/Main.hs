{-# LANGUAGE NoOverloadedStrings #-}
module Main where

import Effectful ()
import Prelude hiding (lookup)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Utils as U
import Servant
import Data.Text (Text)

import Effectful.Servant

type Api1 = "api" :> "v1" :> "name" :> Get '[JSON] Text

server1 :: 

main :: IO ()
main = defaultMain $ testGroup "servant-effectful"
  [ ]
