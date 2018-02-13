module Eval1Spec (spec) where

import           Interpreter
import           Syntax

import           Test.Hspec

spec :: Spec
spec = do
  describe "first interpreter" $
    it "(λx. x) 1 ==> 1" $ do
      let term = App (lam "x" (Var "x")) (Num 1)
      let evaled = eval term
      fst evaled `shouldBe` (Right (Num 1))
      --let term2 = App (lam "x" (App (lam "y" (Var "z")) (Num 2))) (Num 1)
