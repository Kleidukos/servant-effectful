{-# LANGUAGE NoOverloadedStrings #-}

module Main where

import Test.Tasty
import Prelude hiding (lookup)

main :: IO ()
main = defaultMain $ testGroup "servant-effectful" []
