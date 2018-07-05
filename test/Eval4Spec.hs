module Eval4Spec (spec) where

import Figures.Two
import Figures.Four

import GHC.Exts
import Test.Hspec

spec :: Spec
spec = do
  describe "dead code collector" $ do
    it "should work for ifs" $ do
      let term = If 1 420 666
      evalDead term `shouldBe` (Right (Num 420), fromList [666])

    it "should work for lambdas" $ do
      let term = Lam "x" (Var "x")
      evalDead term `shouldBe` (Right (Closure term mempty), fromList [Var "x"])

    it "should still work in case of failure" $ do
      let term = If (Op Div 1 0) 2 3
      evalDead term `shouldBe` (Left DivideByZero, fromList [2, 3])
