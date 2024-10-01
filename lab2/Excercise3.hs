-- Exercise 3
-- Find the definition of the minimal property subset in the lecture. Implement a function that
-- calculates the minimal property subsets, given a 'function under test' and a set of properties.
-- Deliverables: implementation, documentation of approach, indication of time spent.

-- According to the slides:
-- "If a property kills no mutant, or it kills the same mutants as another property, it can be
-- considered redundant over other properties." so minimal property subset is a subset without redundant properties.

import Control.Monad (filterM)
import Data.List (subsequences)
import MultiplicationTable
import Mutation
import Test.QuickCheck


namedProperties :: [(String, [Integer] -> Integer -> Bool)]
-- we need it for representation of properties
namedProperties =
  [ ("prop1", prop_tenElements),
    ("prop2", prop_firstElementIsInput),
    ("prop3", prop_sumIsTriangleNumberTimesInput),
    ("prop4", prop_linear),
    ("prop5", prop_moduloIsZero)
  ]

isSurvivor ::
  [[Integer] -> Integer -> Bool] ->
  (Integer -> [Integer]) ->
  [[Integer] -> Gen [Integer]] ->
  Integer ->
  IO Bool
  -- this function tests if a mutant survived property set
isSurvivor properties fut mutatorsList input = do
  results <- generate $ mapM (\mutator -> mutate' mutator properties fut input) mutatorsList
  -- if any property fails to kill a mutant, set killed to False
  let killed = any (any not) results
  return (not killed)

minimalPropertySubset ::
  [(String, [Integer] -> Integer -> Bool)] ->
  (Integer -> [Integer]) ->
  [[Integer] -> Gen [Integer]] ->
  IO [(String, [Integer] -> Integer -> Bool)]
  -- we generate all possible subsets of properties and test them with 4000 random tests
  -- if a subset kills all 4000 mutants, we return it
  -- if there is more then one subset that kills all mutants, we return the first one
minimalPropertySubset namedProps fut mutatorsList = do
  -- generate all possible subsets of properties
  let allSubsets = subsequences namedProps
  -- check each subset with 4000 random tests to see if it kills all mutants
  validSubsets <-
    filterM
      ( \subset -> do
          let props = map snd subset -- Extract the functions from the subset
          results <-
            mapM
              ( \_ -> do
                  input <- generate arbitrary
                  isSurvivor props fut mutatorsList input
              )
              [1 .. 4000]
          -- we want all of the tests to return False (killed) to be a valid subset
          return $ all not results
      )
      allSubsets
  -- I assume we want just one subset
  return $ head validSubsets 

-- Main function to test the implementation
main :: IO ()
main = do
  minimalSubsets <- minimalPropertySubset namedProperties multiplicationTable mutators
  let propertyNames = map fst minimalSubsets
  print propertyNames


-- Time spent: 300min
