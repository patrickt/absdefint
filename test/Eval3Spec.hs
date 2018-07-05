module Eval3Spec (spec) where

import Figures.One
import Figures.Two
import Figures.Three

import Test.Hspec

spec :: Spec
spec = do
  describe "trace interpreter" $ do
    it "should work for primitives" $ do
      let term = (3 + 4) * 9
      let logged a = Log a mempty mempty
      let theTrace =
            [ logged ((3 + 4) * 9)
            , logged (3 + 4)
            , logged 3
            , logged 4
            , logged 9
            ]
      evalTrace term `shouldBe` (Right (Num 63), theTrace)
