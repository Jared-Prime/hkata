module TuplesKata.BasicSpec where

import Test.Hspec
import Test.QuickCheck
import TuplesKata.Basic

spec :: Spec
spec = do
  describe "fst" $
    it "accesses the first element in a tuple" $ do
      fst (1,2) `shouldBe` 1
      fst ((3,2),1) `shouldBe` (3,2)
  describe "snd" $
    it "accesses the second element in a tuple" $ do
      snd (1,2) `shouldBe` 2
      snd ((3,2),1) `shouldBe` 1
  describe "zip" $
    it "produces 2-tuples" $ do
      zip [1,2,3] [4..6] `shouldBe` [(1,4), (2,5), (3,6)]
      zip [1,2] ['a'..'c'] `shouldBe` [(1,'a'), (2,'b')]
      zip [1..3] ['a'..'b'] `shouldBe` [(1,'a'), (2,'b')]
      zip [1..] ["cat", "dog", "bird"] `shouldBe` [(1,"cat"), (2,"dog"), (3,"bird")]
  describe "pythagorean triples" $ do
    it "useful representation as a 3-tuple" $
      take 2 pythagorean `shouldBe` [(3,4,5), (6,8,10)]
