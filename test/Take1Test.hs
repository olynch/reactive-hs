{-# LANGUAGE DataKinds #-}

module Take1Test where

import Data.ByteString.Lazy.Char8 (pack)
import Take1
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

ex1 :: Tm TB
ex1 = produce $ do
  let x = Const 1
  let y = Const 2
  z <- let_ $ Add x y
  w <- let_ $ Sub z x
  return $ Eq z w

ex1test :: TestTree
ex1test = goldenVsString "ex1" "test/golden/ex1.output" (pure $ pack $ show ex1)
