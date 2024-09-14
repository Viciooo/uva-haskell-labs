import Test.QuickCheck
-- Write a Haskell function that finds the factorial of an integer input, the specification of the 
-- function should be as follows :
-- factorial :: Integer -> Integer
-- The function should be written by utilising recursion.
-- Create at least two props to test the factorial function. The prop should use a random number 
-- generator using QuickCheck Gen construct.
-- Deliverables: Haskell program, concise test report, indication of time spent.

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

nonNegativeNumber :: Gen Integer
nonNegativeNumber = arbitrary `suchThat` (>= 0)

nonNegativeNumberPair :: Gen (Integer, Integer)
nonNegativeNumberPair = do
    n <- nonNegativeNumber
    m <- arbitrary `suchThat` (> n)
    return (n, m)

positiveNumber :: Gen Integer
positiveNumber = arbitrary `suchThat` (> 0)


prop_factorialDefinition :: Property
prop_factorialDefinition = forAll positiveNumber $ \n -> 
    factorial n == product [1..n]

prop_multiplicative :: Property
-- very similiar prop_factorialDefinition, but looking from different angle
prop_multiplicative = forAll nonNegativeNumberPair $ \(n,m) -> 
    n < m ==> 
    let left = factorial m
        right = factorial n * product [n+1..m]
    in left == right


prop_logarythmic :: Property
-- it will have issue due to floating point precision
-- to mitigate this we will have to introduce an epsilon value
prop_logarythmic = forAll nonNegativeNumber $ \n -> 
    let left = log (fromIntegral (factorial n))
        right = sum [log (fromIntegral x) | x <- [1..n]]
    in left == right

prop_logarythmic' :: Property
-- fixed by introducing epsilon
prop_logarythmic' = forAll nonNegativeNumber $ \n -> 
    let epsilon = 0.0001
        left = log (fromIntegral (factorial n))
        right = sum [log (fromIntegral x) | x <- [1..n]]
    in abs (left - right) < epsilon

main :: IO ()
main = do
    quickCheck prop_factorialDefinition
    quickCheck prop_multiplicative
    quickCheck prop_logarythmic -- failing after random number of tests as expected
    quickCheck prop_logarythmic'

-- Time Spent: 30 min
