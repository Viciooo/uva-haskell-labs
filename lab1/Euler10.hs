module Euler10 where

prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
    where xs = takeWhile (\y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

{-
This functions sums all elements of an ordered that are lower than n
Testing:
This function could be tested by giving it a random ordered list of integers > 0 with
the limit as the last one in the list, i.e. the greatest one, and then with the result
we iterate over the list and subtract every number of the list from the result, the final
value should be 0
-}
sumUntil :: Integer -> Integer -> [Integer] -> Integer
sumUntil s n (x:xs) = if x >= n then s else sumUntil (s+x) n xs

euler10 :: Integer
euler10 = sumUntil 0 2000000 primes