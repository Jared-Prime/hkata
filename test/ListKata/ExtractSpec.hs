module ListKata.ExtractSpec where

import Test.Hspec
import Test.QuickCheck
import ListKata.Extract

spec :: Spec
spec = do
  describe "Extracting a portion of a list" $ do
    it "takes elements from a list" $ do
      take 7 ['a'..'z'] `shouldBe` "abcdefg"
      take 3 [1..5] `shouldBe` [1,2,3]
      take 5 (enumFrom 10) `shouldBe` [10,11,12,13,14]

    it "drops elements from a list" $ do
      drop 5 [1..10] `shouldBe` [6,7,8,9,10]
      drop 3 ['a'..'g'] `shouldBe` "defg"
      drop 6 (enumFromTo 10 20) `shouldBe` [16,17,18,19,20]

    it "can split a collection" $ do
      split [1..10] `shouldBe` [[1,2,3,4,5],[6,7,8,9,10]]
      split [1..5] `shouldBe` [[1,2],[3,4,5]]

    it "takes while a condition holds" $ do
      takeWhile (< 3) [1..10] `shouldBe` [1,2]
      takeWhile (< 8) (enumFromTo 5 15) `shouldBe` [5,6,7]
      takeWhile (== 'a') "abracadabra" `shouldBe` "a"

    it "drops while a condition holds" $ do
      dropWhile (< 5) [1..10] `shouldBe` [5,6,7,8,9,10]
      dropWhile (== 'a') "abracadabra" `shouldBe` "bracadabra"
