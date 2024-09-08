import Test.QuickCheck
import Data.Maybe
import Data.List

prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
    where xs = takeWhile (\y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

sumNth101Primes :: Int -> Integer
-- we can think about it as moving window of 101 lenght
-- `drop n primes` will skip first n primes
-- we have no use for the list, so we make a sum of them right away
sumNth101Primes n = sum(take 101 $ drop n primes)

isPrime :: Integer -> Bool
isPrime n = all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) [2..]
-- taken from lecture slides

consecutive101Prime :: Integer
-- first we create the windows for each n starting from 0
-- then using `find isPrime` we get the left most element that is a prime
-- we have to use fromJust to get the value out of a Just
consecutive101Prime = fromJust $ find isPrime (map sumNth101Primes [0..])


main :: IO ()
main = do
    print consecutive101Prime

-- Time spent: 45min
