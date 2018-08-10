module Test.EightQueens where

import EightQueens

import Data.Function ((&))
import Test.Hspec

spec_eightQueens :: Spec
spec_eightQueens =
  describe "EightQueens" $ do
    let oneStep' = oneStep 8
    it "gives 8 cases in one step" $
      length (initialState & oneStep') `shouldBe` 8
    it "gives 42 cases in two steps" $
      length (initialState & oneStep' >>= oneStep') `shouldBe` 42
    it "gives 92 answers overall" $
      length eightQueens `shouldBe` 92
