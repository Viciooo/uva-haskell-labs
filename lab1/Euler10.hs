module Euler10 where

prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
    where xs = takeWhile (\y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

sumUntil s n (x:xs) = if x >= n then s else sumUntil (s+x) n xs

euler10 :: Integer
euler10 = sumUntil 0 2000000 primes

{- Testing Description

-}  