module Exercise5 where

import Data.List
import Test.QuickCheck
import Mutation

-- From the resources we got, we will be using the FitSpec: refining property sets for functional testing paper 
-- to implement the functions to calculate the conjecture.

-- We first need to get what mutants survives against what properties sets as input (This is done in the other exercises).

-- For better readability (the integers represent the index of properties)
type PropertySet = [Int]

-- Once we have our input then we can calculate the conjecture. as follow
-- Case one: if they are equivalent 
-- Case two: if one implies the other

-- To get properties that are equivalent we need to group property sets in to equivalence classes. Two sets are equivalent if they kill the same mutants.
-- Function to check if two property sets are equivalent
-- In this function we go through all possible pairs of tuples from our input and for each pair we check if the 
-- Boolean lists are equal and if the property sets are different. If this is the case then they are equivalent and included in the output.
-- And since there can duplicates in the output we use nub to remove them.
apparentEquivalence :: [(PropertySet, [Bool])] -> [(PropertySet, [Bool])]
apparentEquivalence results =
  nub [(ps1, res1) | (ps1, res1) <- results, (ps2, res2) <- results, res1 == res2, ps1 /= ps2]


-- To find out if a set implies another set we need to check if whenever one mutant survives the first set then it also
-- survives the second set. then the first set implies the second set.
-- Here again we go through all possible pairs of tuples from our input and for each pair we check if the
-- first set implies the second set. We do this be recursivly, we first check the base case if both lists are empty
-- then in recursive case we check if the first element of the first list implies the first element of the second list 
-- implies is be defined as not a or b (source: https://mathworld.wolfram.com/Implies.html)
-- and then we call the function recursively with the rest of the lists.
-- We also have a case where the lists are of different lengths, then we return False.
-- Finally we also check if the property sets are different 
-- if this is the case then we include them in the output.
apparentImplication :: [(PropertySet, [Bool])] -> [(PropertySet, PropertySet)]
apparentImplication results =
  [(ps1, ps2) | (ps1, res1) <- results, (ps2, res2) <- results, res1 `implies` res2, ps1 /= ps2]
  where
    implies :: [Bool] -> [Bool] -> Bool
    implies [] [] = True
    implies (a:as) (b:bs) = (not a || b) && implies as bs
    implies _ _ = False 

-- for example if we have the following input: result = [([1], [False,True,False]), ([2], [False, True, False]), ([1,2], [False, True, True]), ([3], [True,False,False])]
-- then apparentEquivalence result = [([1],[False,True,False]), ([2],[False,True,False])] implying the are equivalent
-- and apparentImplication result = [([1],[2]),([1],[1,2]),([2],[1]),([2],[1,2])] with the first set implying the second set.

-- Time spent: 480min