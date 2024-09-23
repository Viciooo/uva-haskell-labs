import Data.List
import System.Random
import Test.QuickCheck

-- 1.  Implement all properties from the Exercise 3 from Workshop 2 as Haskell functions of type 
-- Int -> Bool . Consider a small domain like [(−10)..10]
-- 2.  Provide a descending strength list of all the implemented properties.

-- Deliverables: Implementation of properties, descending strength list of said 
-- implementation(figure out how to print them, and provide your answer in a comment too), an 
-- indication of time spent

-- =================GIVEN FUNCTIONS========================
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

-- =================GIVEN FUNCTIONS END========================

isStronger :: (Int -> Bool) -> (Int -> Bool) -> [Int] -> Bool
-- It's more convenient to take a definition of stronger from this stronger, weaker function
isStronger p q xs = forall xs (\x -> p x --> q x)

whichIsStronger :: [Int] -> (Int -> Bool) -> (Int -> Bool) -> String
whichIsStronger xs p q
    | stronger xs p q && weaker xs p q = "equally strong"
    | stronger xs p q = "left is stronger"
    | weaker xs p q = "right is stronger"
    | otherwise = "incomparable"

-- (\ x -> even x && x > 3) or even
firstLeft :: Int -> Bool
firstLeft x = even x && x > 3
firstRight :: Int -> Bool
firstRight = even

-- (\ x -> even x || x > 3) or even
secondLeft :: Int -> Bool
secondLeft x = even x || x > 3
secondRight :: Int -> Bool
secondRight = even

-- (\ x -> (even x && x > 3) || even x) or even
thirdLeft :: Int -> Bool
thirdLeft x = (even x && x > 3) || even x
thirdRight :: Int -> Bool
thirdRight = even

-- even or (\ x -> (even x && x > 3) || even x)
-- it's a duplicate but we were to implement all of them - so here we are
fourthLeft :: Int -> Bool
fourthLeft = even
fourthRight :: Int -> Bool
fourthRight x = (even x && x > 3) || even x


-- just for clarity reasons I will now wrap
-- in functions that return the same result but have more reasonable name
-- By no means this is optimal, but we are experimenting here
propertyA :: Int -> Bool
propertyA = firstLeft

propertyB :: Int -> Bool
propertyB = even

propertyC :: Int -> Bool
propertyC = secondLeft

propertyD :: Int -> Bool
propertyD = thirdLeft

propertyE :: Int -> Bool
propertyE = fourthRight

data NamedFunction = NamedFunction {
    functionName :: String,
    function :: Int -> Bool
}

namedFunctions :: [NamedFunction]
-- Since functions don't have names in haskell we will have to walk around it
-- by introducing a mapping function
namedFunctions = [
    NamedFunction "even x && x > 3" propertyA,
    NamedFunction "even" propertyB,
    NamedFunction "even x || x > 3" propertyC,
    NamedFunction "(even x && x > 3) || even x" propertyD,
    NamedFunction "(even x && x > 3) || even x" propertyE
    ]


-- Comparator for NamedFunction based on strength
compareStrength :: [Int] -> NamedFunction -> NamedFunction -> Ordering
compareStrength xs (NamedFunction _ f) (NamedFunction _ g)
  | isStronger f g xs && isStronger g f xs = EQ
  | isStronger f g xs = GT
  | isStronger g f xs = LT
  | otherwise = EQ

-- Sort the named functions based on strength
sortedNamedFunctions :: [NamedFunction]
sortedNamedFunctions = sortBy (compareStrength [(-10)..10]) namedFunctions

-- Extract the names of the sorted functions
descendingStrengthList :: [String]
descendingStrengthList = map functionName sortedNamedFunctions

main :: IO ()
main =
    do
    print $ whichIsStronger [(-10)..10] firstLeft firstRight
    print $ whichIsStronger [(-10)..10] secondLeft secondRight
    print $ whichIsStronger [(-10)..10] thirdLeft thirdRight
    print $ whichIsStronger [(-10)..10] fourthLeft fourthRight

    print descendingStrengthList

-- Time spent: 90min
