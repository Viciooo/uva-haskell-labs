-- Time Spent: 45 min

module Exercise2 where
import Test.QuickCheck

naturalGen :: Gen Integer
naturalGen = suchThat arbitrary (>0)

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = subsequences xs ++ map (x:) (subsequences xs)

intListProperty :: Integer -> Bool
intListProperty n = length ( subsequences [1..n]) == (2 ^ n)

testProperty = quickCheck $ forAll naturalGen intListProperty

{-
Questions:
1. I do believe the property is hard to test because the universe of possible inputs
is infinite, which makes it impossible to extensively test.

2. When performing these tests we can observer that no sort of mathematical
induction is being done to test the mathematical fact, but instead the property 
is being tested against a random subset of the input universe so that an aproximation
of it's correctness can be derived
-}