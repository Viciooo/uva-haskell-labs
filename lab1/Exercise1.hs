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

main :: IO ()
main =  do
    print "=== Divide property ==="
    quickCheck isDevidedProperty 
    print "=== Definition property ==="
    quickCheck factorialDefinition 

-- Test report: Function was tested on two properties: 
    -- 1. Is the result divisible by every number from 1 to x:
    -- This must hold because the factorial is the multiplication of this numbers.

    -- 2. Is the result equal to the number calculated using definition of factorial (without using recursion)
    -- The result should match the factorial calculated by the non-recursive definition

    -- Properties where tested using randomly generated numbers with QuickCheck's Gen.
    -- There was contraint applied onto number genetrator: generated numbers are only non negative integers
    -- This contraint was created becuse factorial by definiton is applied to non negative integers

    -- All the tested passed

-- Time Spent: 30 min