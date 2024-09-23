import Test.QuickCheck
import Data.List

prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
    where xs = takeWhile (\y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

reversal :: Integer -> Integer
reversal = read . reverse . show

fixedReversal :: Integer -> Integer
fixedReversal n
  | n `mod` 10 == 0 = n
  | otherwise       = read . reverse . show $ n

reversibleStream :: [Integer]
reversibleStream = filter (\x -> prime x && prime (reversal x)) [1..10000]

fixedReversalCorrectness :: Property
fixedReversalCorrectness = forAll (arbitrary `suchThat` (\x -> x > 0 && x <= 10000)) (\x -> x == fixedReversal (fixedReversal x))

reversalCorrectness :: Property
-- every number between 0 and 10000 should be equal to reversal of its reversal
reversalCorrectness = forAll (arbitrary `suchThat` (\x -> x > 0 && x <= 10000)) (\x -> x == reversal (reversal x))

primeMembership :: Property
-- every element has to be prime and it's reversal should be in the list
primeMembership = forAll (elements reversibleStream) (\x -> prime x && reversal x `elem` reversibleStream)

reversablePrimeCount :: Bool
-- we know that there are exactly 260 such values and we hardcode it,
-- so we could iterate on the solution in the future with high confidence.
reversablePrimeCount = length reversibleStream == 260

uniqueElements :: Property
-- we compare reversible stream with itself run through deduplication function
uniqueElements = property $ length reversibleStream == length (nub reversibleStream)



main :: IO ()
main = do
    quickCheck reversalCorrectness -- it's failing for numbers with 0 as last digit
    -- this is a clear issue with the reversal function, some numbers just can't be reversed.
    -- We could skip those to have a property of reversal correctness:
    quickCheck fixedReversalCorrectness

    quickCheck primeMembership
    quickCheck uniqueElements
    print reversablePrimeCount -- not a property, can't run it using quickcheck
