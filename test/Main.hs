{-# LANGUAGE DataKinds #-}

module Main (main) where

import Take1Test (ex1test)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup
    "golden tests"
    [ ex1test
    ]

main :: IO ()
main = defaultMain tests
