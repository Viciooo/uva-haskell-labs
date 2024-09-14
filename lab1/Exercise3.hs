import Data.List
import System.Random
import Test.QuickCheck

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

-- (\ x -> even x && x > 3) or even
firstLeft :: Int -> Bool
firstLeft x = even x && x > 3
firstRight :: Int -> Bool
firstRight = even

whichIsStronger :: [Int] -> (Int -> Bool) -> (Int -> Bool) -> String
whichIsStronger xs p q
    | stronger xs p q && weaker xs p q = "equally strong"
    | stronger xs p q = "left is stronger"
    | weaker xs p q = "right is stronger"
    | otherwise = "incomparable"

-- (\ x -> even x || x > 3) or even
secondLeft :: Int -> Bool
secondLeft x = (even x && x > 3) || even x
secondRight :: Int -> Bool
secondRight = even

-- even or (\ x -> (even x && x > 3) || even x)
thirdLeft :: Int -> Bool
thirdLeft = even
thirdRight :: Int -> Bool
thirdRight x = (even x && x > 3) || even x


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
propertyD = thirdRight

data NamedFunction = NamedFunction {
    functionName :: String,
    function :: Int -> Bool
}

namedFunctions :: [NamedFunction]
-- Since functions don't have names in haskell we will have to walk around it
-- by introducing a mapping function
namedFunctions = [
    NamedFunction "propertyA" propertyA,
    NamedFunction "propertyB" propertyB,
    NamedFunction "propertyC" propertyC,
    NamedFunction "propertyD" propertyD
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

    print descendingStrengthList
