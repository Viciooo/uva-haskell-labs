module Exercise1 where
import Mutation
import MultiplicationTable
import Test.QuickCheck
import Data.List

{-
These mutators as whole cover all the properties, due to 'anyList', which
returns a completely different list, so most of the time, it will
fail all the properties.
In the (very) rare case where anyList returns the original list,
addElements will also affect all the defined properties.
-}

{- The first element is not the input
This mutator changes the first element to be different than the output
Ìt affectes the following properties:
    - prop_firstElementIsInput;
    - prop_sumIsTriangleNumberTimesInput;
    - prop_linear;
    - prop_moduloIsZero;
-}
firstElemChange :: [Integer] -> Gen [Integer]
firstElemChange (x:xs) = do 
    num <- arbitrary `suchThat` (\y -> y > 1 && y /= x )
    return $ x+num : xs 

{- Change a random number of the list, excluding the first
This mutator changes a random element of the list, excluding the first
Ìt affectes the following properties:
    - prop_sumIsTriangleNumberTimesInput;
    - prop_linear;
    - prop_moduloIsZero;
-}
randomElemChange :: [Integer] -> Gen [Integer]
randomElemChange xs = do 
    i <- choose (0, length xs - 1) 
    num <- arbitrary `suchThat` (\v -> v `mod` head xs /= 0)
    return $ take i xs ++ (xs !! i) + num : drop(i+1) xs 

{-Sum the first element to all
This mutator will add the input element to all in the list
Ìt affectes the following properties:
    - prop_firstElementIsInput
    - prop_sumIsTriangleNumberTimesInput;
-}
inputSum :: [Integer] -> Gen [Integer]
inputSum xs = return [ x + head xs | x <- xs]

{- Change the order of the elements
This mutator change the order in which the element on the list are shown
Ìt affectes the following properties:
    - prop_firstElementIsInput;
    - prop_linear;
-}
orderChange :: [Integer] -> Gen [Integer]
orderChange xs = do 
    let perms = permutations xs
    i <- choose (1, length xs) 
    return $ perms !! i

-- Return the list of mutators updated 
allMutators :: [[Integer] -> Gen [Integer]]
allMutators = mutators ++  [firstElemChange, randomElemChange, inputSum, orderChange]
