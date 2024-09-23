-- Time Spent: 30 min
module Exercise8 where

prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
    where xs = takeWhile (\y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

counterexamples ::  [([Integer], Integer)]
counterexamples = [(xs, v) | x <- [1..], 
                            let xs = take x primes, -- List of primes in testing
                            let v = product xs + 1, -- Do the product of the primes
                            not $ prime v] -- Check if the result is prime

