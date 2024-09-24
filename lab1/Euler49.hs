module Euler49 where

import Data.List

prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
    where xs = takeWhile (\y -> y^2 <= n) primes

primes :: [Integer]
primes = filter prime [1000..9999]
 
{-
Check if a number is a permutation of another
Testing: 
-}
permutableNumbers :: Integer -> Integer -> Bool
permutableNumbers x y = sort (show x) == sort (show y)

{-
Check if two numbers are part of the triple
-}
permutableTriple x y = do
    let z = y + (y-x)
    permutableNumbers x y && permutableNumbers x z && prime z

find3 = [(x,y) | x <- primes, 
                y <- primes,
                let next = y + (y - x),
                permutableTriple x y]
