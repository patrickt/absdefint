module Eval1Spec (spec) where

import           Figures.One
import           Figures.Two

import GHC.Exts
import           Test.Hspec

spec :: Spec
spec = do
  describe "first interpreter" $ do
    it "(λx. x) 1 ==> 1" $ do
      let term = App (Lam "x" (Var "x")) 1
      eval term `shouldBe` (Right (Num 1))

    it "(λ x . let y = x + 1 . y) 1 ==> 2" $ do
      let term = App (Lam "x" (let' "y" ((Var "x") + 1) (Var "y"))) 1
      eval term `shouldBe` (Right 2)

    it ("/ 1 0 ==> error") $ do
      let term = Op Div 1 0
      eval term `shouldBe` (Left DivideByZero)

    it "(λx. (λy. y)) 1 ==> closure with x" $ do
      let inner = Lam "y" (Var "y")
      let term = App (Lam "x" inner) (Num 1)
      let env = fromList [("x", 0)]
      eval term `shouldBe` Right (Closure inner env)

let' :: Name -> Exp -> Exp -> Exp
let' name val bod = App (Lam name bod) val
