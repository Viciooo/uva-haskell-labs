module Lab0 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- forall :: [a] -> (a -> Bool) -> Bool
-- forall = flip all

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

firstBucketCounter :: [Float] -> Int
firstBucketCounter xs = length [x | x <- xs, x <0.25]

secondBucketCounter :: [Float] -> Int
secondBucketCounter xs = length [x | x <- xs, 0.25 <= x, x < 0.50]

thirdBucketCounter :: [Float] -> Int
thirdBucketCounter xs = length [x | x <- xs, 0.50 <= x, x < 0.75]

fourthBucketCounter :: [Float] -> Int
fourthBucketCounter xs = length [x | x <- xs, 0.75 <= x, x < 1]

-- number of values, number of values in bucket
deviation :: Int -> Int-> Bool
deviation x y = let z = fromIntegral x :: Float
                    expected = round (z / 4)
                    margin = round (z * 0.01)
                  in expected - margin <= y && y <= expected + margin

-- deviation x y = (((round (x / 4)) - x * 0.01) <= y && y <= (round (x / 4) + x * 0.01))


isEquall :: [Float] -> Bool
isEquall xs = let
             first = firstBucketCounter xs 
             second = secondBucketCounter xs 
             third = thirdBucketCounter xs 
             fourth = fourthBucketCounter xs 
             in (deviation 10000 first 
                    && deviation 10000 second
                    && deviation 10000 third
                    && deviation 10000 fourth)



main :: IO ()
main = do
    randomFloats <- probs 10000  -- Generate a list of 1000 random floats
    let count1 = firstBucketCounter randomFloats
    let count2 = secondBucketCounter randomFloats
    let count3 = thirdBucketCounter randomFloats
    let count4 = fourthBucketCounter randomFloats

    let result = isEquall randomFloats  -- Count how many are in the first bucket
    print result
    -- print count1
    -- print count2
    -- print count3
    -- print count4
