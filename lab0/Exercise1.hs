
module Lab0 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
-- Take into consideration that the float can be created
-- Time 1 hour
left :: Integer -> Integer
left n = sum [n^2 | n <- [1..n]]

right :: Integer -> Integer
right n = n*(n+1)*(2*n+1) `div` 6

result :: Integer -> Bool
result n = left n == right n

genPositiveInt :: Gen Integer
genPositiveInt = (arbitrary :: Gen Integer) `suchThat` (> 1)


left2 :: Integer -> Integer
left2 n = sum [n^3 | n <- [1..n]]

right2 :: Integer -> Integer
right2 n = (n*(n+1) `div` 2)^2
-- result_prop :: Property
-- result_prop = forAll genPositiveInt $ \n -> result n

main :: IO ()
main = quickCheck $ forAll genPositiveInt result

