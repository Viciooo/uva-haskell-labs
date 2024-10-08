-- Time Spent: 360 min
module Exercise3 where

{-
Find the definition of the minimal property subset in the lecture. 
Implement a function that calculates the minimal property subsets, 
given a 'function under test' and a set of properties.
Deliverables:
    - implementation,
    - documentation of approach,
    - indication of time spent.
-}

import Test.QuickCheck
import Mutation
import Control.Monad

type Mutator = [Integer] -> Gen [Integer]
type ListProperty = [Integer] -> Integer -> Bool
type Fut = Integer -> [Integer]

mutatorBattle :: Fut -> Integer -> [ListProperty] -> [Mutator] -> [(Mutator, Gen [Bool])]
mutatorBattle fut input properties mutators = [(m, life) | m <- mutators, let life = mutate' m properties fut input]

-- Generate a list where the index represents a mutator < the content of each element is a 
-- list of indexes of properties from the original list
killersMap :: Fut -> Integer -> [ListProperty] -> [Mutator] -> IO [[Int]]
killersMap fut input properties mutators = do
    -- List of tuple where the first element is a mutator and the second is a list of bool
    -- The index of the element on the list is the same as the index of its Mutator on the mutator list
    -- Each bool represents a propertie, with the link being made by both sharing the same index 
    -- The value of the bool determines whether the propertie killed the mutant or not
    -- Filter out equivalent mutants, i.e. with empty bool list
    let ms = filterM (snd >=> (return . not . null)) $ mutatorBattle fut input properties mutators
    mutatorStats <- generate ms
    -- Converts the previous list in one list of lists, with the index representing the mutator 
    -- and the list the index of the properties that killed it
    let killers = [ li | b <- mutatorStats, let li = snd b >>= \x -> return $ [ snd i | i <- zip x [0..], not $ x !! snd i]]
    mapM generate killers


-- Generate possible combinations of the minimumSubset by selecting one elemnet from the first list
-- and filtering the remaining lists so that it only remains lists without the selected element
subsets :: [[Int]] -> [[Int]]
subsets (l:lls) = do
    i <- l
    let flls = filter (notElem i) lls
    if null flls then [[i]] else map (i :) $ subsets flls

minimumSubSets :: [[Int]] -> [[Int]]
minimumSubSets ll = do
    let sub = subsets ll
    -- Get the size of the minimum subset
    let min = minimum $ map length sub
    -- Filter the subsets by the size of the minimum one
    filter (\l -> min == length l) sub

calculatMinimumSubSets :: Fut -> Integer -> [ListProperty] -> [Mutator] ->IO [[Int]]
calculatMinimumSubSets fut input properties mutators = do
    let km = killersMap fut input properties mutators
    km >>=
        \x ->
            if any null x then return [] -- Check if all mutators are killed
            else minimumSubSets <$> km