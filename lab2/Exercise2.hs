-- Time Spent: 180 min
module Exercise2 where

import Test.QuickCheck
import Mutation
import MultiplicationTable
import Data.Foldable (foldlM)

type Mutator = [Integer] -> Gen [Integer]
type ListProperty = [Integer] -> Integer -> Bool
type Fut = Integer -> [Integer]

randomInteger :: Gen Integer
randomInteger = arbitrary

-- Choose a random mutator and test it against the properties
-- Returns True if the mutator is still alive
battle :: Fut -> [ListProperty] -> [Mutator] -> IO Bool
battle fut properties mutators = do
    i <- generate $ choose (0, length mutators - 1)
    num <- generate randomInteger
    res <- generate $ mutate' (mutators !! i) properties fut num
    let alive = not (null res) && and res

    return $ alive

countSurvivors :: Integer -> Fut -> [ListProperty] -> [Mutator] -> IO Integer
countSurvivors 0 fut properties mutators = return 0
countSurvivors numMutants fut properties mutators = do
    battles <- sequence $ [battle fut properties mutators | b <- [1..numMutants]]

    -- The number of survivors are the number of True in the list
    return $ fromIntegral $ length $ filter id battles

{-
The following tests are done with a prop_firstElementIsInput modified.
The provided implementation calls the head of the list without checking if the list is empty, 
which was causing an exception. 
-}
prop_firstElementIsInputFix :: [Integer] -> Integer -> Bool
prop_firstElementIsInputFix o i = (not $ null o) && head o == i

-- The number of survivors using all the properties and mutators is 0
testSurvivorsAll = countSurvivors 4000 multiplicationTable multiplicationTableProps mutators

-- Removing the first two propertie makes the count of survivors go from 0 to a range of 15-30 (+-5)
multiplicationTableProps1 = [prop_sumIsTriangleNumberTimesInput, prop_linear, prop_moduloIsZero]
testSurvivors1 = countSurvivors 4000 multiplicationTable multiplicationTableProps1 mutators

-- Keeping only the first two properties puts the count of survivers in a range of 35-50 (+-5)
multiplicationTableProps2 = [prop_tenElements, prop_firstElementIsInputFix]

testSurvivors2 = countSurvivors 4000 multiplicationTable multiplicationTableProps2 mutators

-- Using only these two we obtain a survivor count of 0, which means they must be a minimal subset
-- because from the previous tests, when separate they can't kill all mutants
multiplicationTableProps3 = [prop_tenElements, prop_sumIsTriangleNumberTimesInput]

testSurvivors3 = countSurvivors 4000 multiplicationTable multiplicationTableProps3 mutators

-- These 3 properties return a number of survivors >1000
multiplicationTableProps4 = [prop_linear, prop_moduloIsZero, prop_firstElementIsInputFix]

testSurvivors4 = countSurvivors 4000 multiplicationTable multiplicationTableProps4 mutators

{-
From the previous tests we can clearly see that prop_tenElements, prop_sumIsTriangleNumberTimesInput are much stronger
than prop_linear, prop_moduloIsZero, prop_firstElementIsInput, by number of the survivors when they are present and
when they aren't
-}