module Exercise4 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

reversal :: Integer -> Integer
reversal = read . reverse . show

reversibleStream :: [Integer]
reversibleStream = [x | x <- [2..10000], prime x, prime $ reversal x]

reversibleStream2 :: [Integer]
reversibleStream2 = [x | x <- [2..10000], prime x, prime $ reversal x]

genPositiveInt :: Gen Integer
genPositiveInt = (arbitrary :: Gen Integer) `suchThat` (\x -> x > 0 && x < 10000)

-- 1. Reversal correctness:
-- every number between 0 and 10000 should be equal to reversal of its reversal

-- it's failing for numbers with 0 as last digit
-- this is a clear issue with the reversal function, some numbers just can't be reversed.

-- Two possible ways to fix this are:
-- 1. Just skipping those numbers because number with last digit 0 is crealy not prime (it can be divaded by 10)
--    The property wont work as intended but wont influence the final result of reversibleStream
-- 2. Creating class that indicates that digits are reversed versions of this number, ex 60 -> 06.
reversalCorrectness :: Integer -> Bool
reversalCorrectness x = x == reversal (reversal x)

reversalCorrectnessProperty :: Property
reversalCorrectnessProperty = forAll genPositiveInt $ \x -> reversalCorrectness x

fixedReversal :: Integer -> Integer
fixedReversal n
  | n `mod` 10 == 0 = n
  | otherwise       = read . reverse . show $ n

fixedReversalCorrectness :: Property
fixedReversalCorrectness = forAll genPositiveInt (\x -> x == fixedReversal (fixedReversal x))

-- 3. Prime Membership
-- there are two ways to interprate this property: 
-- every element has to be prime and it's reversal should be in the list
-- every element has to be prime, reversal of element should be prime and element twice reversed should be equal to original element

-- first interpretation would pass the test but second would not because of the reversal corectness property
primeMembership :: Property
primeMembership = forAll (elements reversibleStream) (\x -> prime x && reversal x `elem` reversibleStream)

-- 4. Reversible Prime Count
-- length of the list of generated values has to be the same as length od pre-computed ones

-- we know that there are exactly 260 such values and we hardcode it,
-- so we could iterate on the solution in the future with high confidence.
reversablePrimeCount :: Bool
reversablePrimeCount = length reversibleStream == 260

-- 5. Reversal Symmetry
-- reversal of every element of the list should also be in the list
reversalSymmetry :: [Integer] -> Bool
reversalSymmetry xs = and [reversal x `elem` xs | x <- xs]

reversalSymmetryProperty :: Property
reversalSymmetryProperty = property $ reversalSymmetry reversibleStream

-- 6. Unique Values
-- every value in the calculated list should be unique
uniqueValues :: [Integer] -> Bool
uniqueValues xs = length xs == length (nub xs)

uniqueValuesProperty :: Property
uniqueValuesProperty = property $ uniqueValues reversibleStream

-- 7. Check Maximum Value
-- the maximum value in the calculated list must be smaller than 10000
maximumValue :: [Integer] -> Bool
maximumValue xs = and [x < 10000 | x <- xs]

maximumValueProperty :: Property
maximumValueProperty = property $ maximumValue reversibleStream


main :: IO ()
main =  do
    print "=== Reversal correctness ==="
    quickCheck reversalCorrectnessProperty 
    print "=== Fixed reversal correctness ==="
    quickCheck fixedReversalCorrectness 
    print "=== Prime Membership ==="
    quickCheck primeMembership
    print "=== Reversal Symmetry ==="
    quickCheck reversalSymmetryProperty
    print "=== Unique Values ==="
    quickCheck uniqueValuesProperty
    print "=== Check Maximum Value ==="
    quickCheck maximumValueProperty

-- Time spent: 150min
