module ListKata.Comprehension where

squares :: [Integer]
squares = [ x^2 | x <- [1..]]

evenSquares :: [Integer]
evenSquares = filter even squares

oddSquares :: [Integer]
oddSquares = filter odd squares

cubes :: [Integer]
cubes = [ x^3 | x <- [1..]]

evenCubes :: [Integer]
evenCubes = [ x^3 | x <- [1..], even x]

oddCubes :: [Integer]
oddCubes = [ x^3 | x <- [1..], odd x]

squaresAndCubes :: [Integer]
squaresAndCubes = [ n | x <- [1..], n <- [x^2, x^3] ]

upperCaseLetters :: [Char]
upperCaseLetters = [ a | a <- ['A'..'Z']]

upperCaseFrom :: [Char] -> [Char]
upperCaseFrom str = [ a | a <- str, t <- upperCaseLetters, a == t ]
