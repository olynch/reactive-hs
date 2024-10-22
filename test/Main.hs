{-# LANGUAGE DataKinds #-}

module Main (main) where

import Data.ByteString.Lazy.Char8 (pack)
import MyLib
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)

ex1 :: Tm TB
ex1 = produce $ do
  let x = Const 1
  let y = Const 2
  z <- let_ $ Add x y
  w <- let_ $ Sub z x
  return $ Eq z w

tests :: TestTree
tests =
  testGroup
    "gen golden tests"
    [ goldenVsString "ex1" "test/golden/ex1.output" (pure $ pack $ show ex1)
    ]

main :: IO ()
main = defaultMain tests
