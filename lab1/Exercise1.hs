-- Time Spent: 30 min

module Exercise1 where
import Test.QuickCheck

naturalGen :: Gen Integer
naturalGen = suchThat arbitrary (>0)

factorial :: Integer -> Integer
factorial n 
    |n == 0 = 1
    |otherwise = n * factorial (n-1)

{- 
Property 1
A factorial should be divisible by all numbers between n and 1
-}

divisibleProp :: Integer -> Bool
divisibleProp n = all (\x -> factorial n `mod` x == 0) [1..n]

testDivisible :: IO ()
testDivisible = quickCheck $ forAll naturalGen divisibleProp

{-
Property 2
Divide by all number between n-1 and 1 should give the  
original number
-}

findOriginal :: Integer -> Integer -> Integer -> Integer
findOriginal v i n 
    | i >= n = v
    | otherwise = findOriginal (v `div` i) (i+1) n 

originalExtraction :: Integer -> Bool
originalExtraction n = n == findOriginal (factorial n) 1 n

testOriginalExtraction :: IO ()
testOriginalExtraction =  quickCheck $ forAll naturalGen originalExtraction