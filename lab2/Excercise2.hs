-- Write a function countSurvivors that counts the number of survivors:
-- countSurvivors :: Integer -> [([Integer] -> Integer -> Property)] -> (Integer -> [Integer]) -> IO Integer
-- Where the first argument is the number of mutants (4000 in the FitSpec example), the second
-- argument is the list of properties, and the third argument is the function under test (the
-- multiplication table function in this case).
-- The output is the number of surviving mutants (0 in the FitSpec example).
-- Document the effect of which mutations are used and which properties are used on the number of
-- survivors.

-- Hints:
-- 1. Consider the relation between properties and survivors.
-- 2. The above-mentioned function definition is not final. Feel free to modify it, for example by
-- adding the mutations that should be used.
-- Deliverables: implementation, documentation of approach, effect of using different
-- mutators/properties, indication of time spent.
module Exercise2 where

import Control.Monad (filterM)
import Data.List
import MultiplicationTable
  ( multiplicationTable,
    multiplicationTableProps,
    prop_firstElementIsInput,
    prop_linear,
    prop_moduloIsZero,
    prop_sumIsTriangleNumberTimesInput,
    prop_tenElements,
  )
import Mutation
import Test.QuickCheck

countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]] -> IO Integer
countSurvivors nMutants properties fut mutatorsList = do
  survivors <- filterM (isSurvivor properties fut mutatorsList) [1 .. nMutants]
  return $ fromIntegral (length survivors)

isSurvivor :: [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]] -> Integer -> IO Bool
isSurvivor properties fut mutatorsList input = do
  results <- generate $ mapM (\mutator -> mutate' mutator properties fut input) mutatorsList
  let killed = any (any not) results
  return (not killed)

main :: IO ()
main = do
  -- Testing different mutators for all properties
  let allMutators = mutators -- the more mutators we apply, the more mutants will be killed, it's better to apply them seperatly
  let justAnyListMutator = [anyList]
  let justRemoveElementsMutator = [removeElements]
  let justAddElementsMutator = [addElements]
  survivors1 <- countSurvivors 40000 multiplicationTableProps multiplicationTable allMutators
  print survivors1
  survivors2 <- countSurvivors 40000 multiplicationTableProps multiplicationTable justAnyListMutator
  print survivors2
  survivors3 <- countSurvivors 40000 multiplicationTableProps multiplicationTable justRemoveElementsMutator
  print survivors3
  survivors4 <- countSurvivors 40000 multiplicationTableProps multiplicationTable justAddElementsMutator
  print survivors4

  let props1 = [prop_tenElements]
  let props2 = [prop_firstElementIsInput]
  let props3 = [prop_sumIsTriangleNumberTimesInput]
  let props4 = [prop_linear]
  let props5 = [prop_moduloIsZero]
  let props12 = [prop_tenElements, prop_firstElementIsInput]
  survivors5 <- countSurvivors 40000 props1 multiplicationTable allMutators
  print survivors5
  survivors6 <- countSurvivors 40000 props2 multiplicationTable allMutators
  print survivors6
  survivors7 <- countSurvivors 40000 props3 multiplicationTable allMutators
  print survivors7
  survivors8 <- countSurvivors 40000 props4 multiplicationTable allMutators
  print survivors8
  survivors9 <- countSurvivors 40000 props5 multiplicationTable allMutators
  print survivors9

-- the more properties used, the less mutants will survive

-- Time spent: 180min
