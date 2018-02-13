module Eval1Spec (spec) where

import           Interpreter
import           Syntax

import           Test.Hspec

spec :: Spec
spec = do
  describe "first interpreter" $ do
    it "(λx. x) 1 ==> 1" $ do
      let term = App (lam "x" (Var "x")) (Num 1)
      let evaled = eval term
      fst evaled `shouldBe` (Right (Num 1))
    
    it "(λ x . let y = x + 1 . y) 1 ==> 2" $ do
      let term = App (lam "x" (Let "y" (Op Add (Var "x") (Num 1)) (Var "y"))) (Num 1)
      let evaled = eval term
      fst evaled `shouldBe`  (Right (Num 2))
