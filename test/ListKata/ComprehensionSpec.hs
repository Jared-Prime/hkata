module ListKata.ComprehensionSpec where

import Test.Hspec
import Test.QuickCheck
import ListKata.Comprehension

spec :: Spec
spec = do
  describe "list comprehension" $ do
    it "squares numbers from 1 to 5" $ do
      [ x^2 | x <- [1..5] ] `shouldBe` [1,4,9,16,25]
      take 5 squares `shouldBe` [1,4,9,16,25]
    it "squares even numbers from 1 to 5" $ do
      [ x^2 | x <- [1..5], even x ] `shouldBe` [4,16]
      take 2 evenSquares `shouldBe` [4,16]
    it "squares odd numbers from 1 to 5" $ do
      [ x^2 | x <- [1..5], odd x ] `shouldBe` [1,9,25]
      take 3 oddSquares `shouldBe` [1,9,25]
    it "cubes numbers from 1 to 5" $ do
      [ x^3 | x <- [1..5]] `shouldBe` [1,8,27,64,125]
      take 5 cubes `shouldBe` [1,8,27,64,125]
    it "cubes even numbers from 1 to 5" $ do
      [ x^3 | x <- [1..5], even x] `shouldBe` [8,64]
      take 2 evenCubes `shouldBe` [8,64]
    it "cubes odd numbers from 1 to 5" $ do
      [ x^3 | x <- [1..5], odd x] `shouldBe` [1,27,125]
      take 3 oddCubes `shouldBe` [1,27,125]
    it "squares and cubes numbers from 1 to 5" $ do
      [ n | x <- [1..5], n <- [x^2, x^3] ] `shouldBe` [1,1,4,8,9,27,16,64,25,125]
      take 10 squaresAndCubes `shouldBe` [1,1,4,8,9,27,16,64,25,125]
    it "filters squares and cubes below 50" $ do
      [ n | x <- [1..5], n <- [x^2, x^3], n < 50 ] `shouldBe` [1,1,4,8,9,27,16,25]
      [ n | n <- take 10 squaresAndCubes, n < 50 ] `shouldBe` [1,1,4,8,9,27,16,25]
    it "creates tuples from lists" $
      [ (n,a) | n <- [1..3], a <- ['a'..'b'] ] `shouldBe` [(1,'a'), (1,'b'), (2, 'a'), (2,'b'), (3, 'a'), (3, 'b')]
    it "filters uppercase letters" $ do
      [ a | a <- "aAbBcCdD", t <- ['A'..'Z'], a == t ] `shouldBe` "ABCD"
      [ a | a <- "aAbBcCdD", t <- upperCaseLetters, a == t ] `shouldBe` "ABCD"
    it "generalizes to a function" $
      upperCaseFrom "aAbBcCdD" `shouldBe` "ABCD"
