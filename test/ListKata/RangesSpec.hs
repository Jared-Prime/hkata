module ListKata.RangesSpec where

import Test.Hspec
import Test.QuickCheck
import ListKata.Ranges

spec :: Spec
spec = do
  describe "range syntax and usage" $ do
    it "gives a shorthand for a list" $
      [1..10] `shouldBe` [1,2,3,4,5,6,7,8,9,10]
    it "defines steps" $
      [2,4..10] `shouldBe` [2,4,6,8,10]
  describe "cycling through a range" $
    it "goes forever, so remember to take" $
      take 6 (cycle [1..3]) `shouldBe` [1,2,3,1,2,3]
  describe "repeating an item" $ do
    it "goes forever, so remember to take" $
      take 6 (repeat 'a') `shouldBe` "aaaaaa"
    it "better expressed by replicate" $
      replicate 6 'a' `shouldBe` "aaaaaa"
