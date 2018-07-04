module Eval1Spec (spec) where

import           Figures.One
import           Figures.Two

import           Test.Hspec

lam = Lam

spec :: Spec
spec = do
  describe "first interpreter" $ do
    it "(λx. x) 1 ==> 1" $ do
      let term = App (lam "x" (Var "x")) (Num 1)
      eval term `shouldBe` (Right (Num 1))

    it "(λ x . let y = x + 1 . y) 1 ==> 2" $ do
      let term = App (lam "x" (let' "y" (Op Add (Var "x") (Num 1)) (Var "y"))) (Num 1)
      eval term `shouldBe` (Right (Num 2))

    it ("/ 1 0 ==> error") $ do
      let term = Op Div (Num 1) (Num 0)
      eval term `shouldBe` (Left DivideByZero)

let' :: Name -> Exp -> Exp -> Exp
let' name val bod = App (Lam name bod) val
