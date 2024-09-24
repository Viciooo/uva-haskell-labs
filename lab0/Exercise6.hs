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

consecutive101Prime :: Integer
consecutive101Prime = sum $ take 101 [x | x <- [2..], prime x]

-- no i dont have to mathematiclly it is correct
-- x1 + x2 + .. + x101 < (x1 + y1) + (x2 + y2) + ... + (x101 + y101) where x > 0 and y > 0 