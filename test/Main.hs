{-# LANGUAGE NoOverloadedStrings #-}
module Main where

import Effectful
import Prelude hiding (lookup)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Cache as C
import qualified Utils as U

import Effectful.Cache

main :: IO ()
main = defaultMain $ testGroup "servant-effectful" [ ]
