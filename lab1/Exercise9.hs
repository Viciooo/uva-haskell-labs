-- Time Spent: 60 min
module Exercise9 where
import Data.List
import Test.QuickCheck

-- Check how many times an element is on a list
elemFrequency :: Eq a => a -> [a] -> Integer
elemFrequency v xs = sum [ 1 | x <- xs, x == v]

-- Compare the frequency of an element in the first list is the same
-- as the second list
-- If they are the same, we can also assume the list is the same size
elemFrequencyLists :: Eq a => [a] -> [a] -> Bool
elemFrequencyLists xs ys = and [elemFrequency x xs == elemFrequency x ys | x <- xs]

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys =
    elemFrequencyLists xs ys &&
    or [ x /= y | x <- xs, y <- ys]

{-Properties
With 'a' being any type that implements Eq, we can't use properties
that make assumptions about the type, such as testing of both list ordered
are the same or if the sum of the elements values is the same between the 2
-}

-- Test if both lists have the same length
propertyLength :: Eq a => [a] -> [a] -> Bool
propertyLength xs ys = length xs == length ys

-- Test if both lists have the same unique elements
propertyUniqueElem :: Eq a => [a] -> [a] -> Bool
propertyUniqueElem xs ys = do
    -- Remove all duplicates
    let cxs = nub xs 
    let cys = nub ys
    -- Check for all unique elements of xs that they are in ys
    and [x `elem` ys | x <- xs]

-- Check if the frequency of the elements is the same in both lists
propertyFrequency :: Eq a => [a] -> [a] -> Bool
propertyFrequency xs ys = elemFrequencyLists xs ys

{-Input list with no duplicates

This assumptions removes an important part of the input universe and without it
the testing procedure is incomplete. The testing will not guarantee correctness for
permutations on list with duplicate elements, making the behaviour of the function
unpredictable for this subset of cases
-}

{-Automate test process 
Yes, we can use quickCheck to generate a random list and then create a function
that returns the list with a random permutation. For simplicity
we remove the first 2 lists of the permutations, with the first
being the original and the second a pemutation
The following code assumes the type of 'a' as an Integer
-}

-- Generate list with at least one element is different
genList :: Gen [Integer]
genList = listOf arbitrary `suchThat` (\x -> length x > 1 && length x == length (nub x))

genPermutation :: Gen ([Integer],[Integer])
genPermutation =  do
    xs <- genList
    let perm = permutations xs
    return (perm !! 0, perm !! 1)

testIsPermutation = quickCheck $ forAll genPermutation $ uncurry isPermutation
