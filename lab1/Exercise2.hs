module Exercise2 where

import Data.List 
import System.Random
import Test.QuickCheck

-- The property of of Powerset (P(A)) (Also known as the cardinality of the powerset)
-- is that the number of elements in the powerset of a set A is equal to 2^n, where n is the number of elements in the set A.
-- This is due to the fact that for each element in the set A, there are two possibilities: either the element is in the subset or it is not.
-- So we can check this by simply checking if the length of the powerset of a set A is equal to 2^n, where n is the number of elements in the set A. 
cardinalityProperty :: [Int] -> Bool
cardinalityProperty xs = length (powerset xs) == 2^(length xs)


-- In the powerset function, we first check if the list is empty. This is the base case, and we return [[]], because the powerset of an empty set is a set containing only the empty set (as defined in the powerset wiki).
-- For the recursive case, we split the list into its head (x) and tail (xs). We first compute the powerset of the tail (powerset xs), which gives us all the subsets that do not include the head element.
-- Then, we map the head element to each subset of the tail's powerset (using map (x:)) to generate the subsets that do include the head element.
-- Finally, we concatenate the subsets that include the head with those that don't to form the complete powerset.   
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = (powerset xs) ++ (map (x:) (powerset xs))

-- In our function, the order of the subsets in the powerset is not the same as the order provided in the assignment example. 
-- It was not clear in the assignment whether the powerset's order must be the same as the example, but we did realize that this difference is due to how we split the list.
-- And if we wanted to have the same order as the order given in the example, we could have either
-- reversed the list before applying the powerset function, or
-- change the recursive structure to break the list from everything but the last element instead of splitting by the head and tail.


main :: IO ()
main = do
    quickCheck (forAll (resize 20 arbitrary) cardinalityProperty)
-- Resize 20 is used to limit the size of the list to 20 elements, so that the test does not take too long to run for our testing purposes.
-- As mentioned in question 1, We have the problem of exponential growth in the number of subsets as the sets grow larger, which makes it computationally expensive to generate and count all subsets for large sets.


--Question 1
-- Checking the cardinality property of the powerset is not hard to test since we can simply check if the length of the powerset of a set A is equal to 2^n, where n is the number of elements in the set A.
-- However, as the sets grow larger, we have the issue of exponential growth in the number of subsets, which you can consider hard to test for large sets.
-- Since this exponential growth makes it computationally expensive to generate and count all subsets for large sets.       
-- For example for a set of 10 elements, the powerset will have 2^10 = 1024 subsets meanwhile for a set of 20 elements, the powerset will have 2^20 = 1048576 subsets.

--Question 2
-- The cardinality property of a powerset is a mathematical fact. However, in these tests We are not proving if this is correct or not (this has already been proved in set theory).
-- Instead, we are testing if our implementation of the powerset satisfies this property by checking if it produces the correct number of subsets as specified in the property.
-- Essentially, we are testing if our implementation of the powerset function is correct by ensuring that it behaves as intended based on the specification.

