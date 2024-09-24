module Exercise1 where
import Test.QuickCheck

factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x - 1)

genPositiveInt :: Gen Integer
genPositiveInt = (arbitrary :: Gen Integer) `suchThat` (>= 0)

isDevided :: Integer -> Integer -> Bool
isDevided x y = and [x `mod` z == 0 | z <- [1..y]]

isDevidedProperty :: Property
isDevidedProperty = forAll genPositiveInt (\x -> isDevided (factorial x) x)

factorialDefinition :: Property
factorialDefinition = forAll genPositiveInt (\x -> factorial x == product [1..x])

nonNegativeNumber :: Gen Integer
nonNegativeNumber = arbitrary `suchThat` (>= 0)

nonNegativeNumberPair :: Gen (Integer, Integer)
nonNegativeNumberPair = do
    n <- nonNegativeNumber
    m <- arbitrary `suchThat` (> n)
    return (n, m)


prop_multiplicative :: Property
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
main =  do
    -- properties were taken from https://en.wikipedia.org/wiki/Factorial

    print "=== Divide property ==="
    quickCheck isDevidedProperty 
    print "=== Definition property ==="
    quickCheck factorialDefinition
    print "=== Multiplicative property ==="
    quickCheck prop_multiplicative
    print "=== Logarithmic property with floating point precision issue ==="
    quickCheck prop_logarythmic
    print "=== Logarithmic property ==="
    quickCheck prop_logarythmic'

-- Test report: Function was tested on two properties: 
    -- 1. Is the result divisible by every number from 1 to x:
    -- This must hold because the factorial is the multiplication of this numbers.

    -- 2. Is the result equal to the number calculated using definition of factorial (without using recursion)
    -- The result should match the factorial calculated by the non-recursive definition

    -- 3. Multiplicative property:
    -- The factorial of m should be equal to the factorial of n multiplied by the product of all numbers between n and m.

    -- 4. Logarithmic property:
    -- The logarithm of the factorial of n should be equal to the sum of the logarithms of all numbers between 1 and n.


-- Time Spent: 60 min
