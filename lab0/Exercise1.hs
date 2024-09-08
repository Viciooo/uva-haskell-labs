module Exercise1 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- first Equation

leftFirstEquation :: Int -> Int
leftFirstEquation n = sum [x^2 | x <- [1..n]]

rightFirstEquation :: Int -> Int
rightFirstEquation n = n*(n+1)*(2*n+1) `div` 6

firstEquation :: Int -> Bool
firstEquation n = leftFirstEquation n == rightFirstEquation n

-- second Equation

leftSecondEquation :: Int -> Int
leftSecondEquation n = sum [x^3 | x <- [1..n]]

rightSecondEquation :: Int -> Int
rightSecondEquation n = (n*(n+1) `div` 2)^2

secondEquation :: Int -> Bool
secondEquation n = leftSecondEquation n == rightSecondEquation n

-- natural numbers generator
naturalNumbers :: Gen Int
naturalNumbers = arbitrary `suchThat` (>= 1) -- some argue that 0 should be included, but we chose to exclude it

propertyfirstEquation :: Property
propertyfirstEquation = forAll naturalNumbers firstEquation

propertySecondEquation :: Property
propertySecondEquation = forAll naturalNumbers secondEquation

main :: IO ()
main = do
    quickCheck firstEquation -- The Equations obviously didn't pass without using the generator. 
    -- Because quickCheck run it against all numbers, inluding negatives
    quickCheck propertyfirstEquation
    quickCheck propertySecondEquation


-- Question: How would you prove that these statements are true for all natural numbers?
-- Answer: You can prove it only using math, however you can test it against a lot of testcases using quickCheck and consider it good enough.

-- Time spent: 15min
