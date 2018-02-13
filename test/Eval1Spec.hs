module Eval1Spec (spec) where

import           Interpreter
import           Syntax

import           Data.Function
import           Test.Hspec

spec :: Spec
spec = do
  describe "first interpreter" $
    it "(λx. x) 1 ==> 1" $ do
      let term = App (lam "x" (Var "x")) (Num 1)
      let eval = execInterp ((fix eval1) term)
      fst eval `shouldBe` (Right (Num 1))
