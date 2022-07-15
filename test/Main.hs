{-# LANGUAGE NoOverloadedStrings #-}
module Main where

import Prelude hiding (lookup)
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "servant-effectful" [ ]
