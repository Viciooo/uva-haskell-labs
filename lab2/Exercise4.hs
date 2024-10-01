module Exercise4 where
{-
Implement a function that calculates the strength of a given set of properties, which is the percentage of mutants they kill.
Deliverables: 
    - implementation,
    - documentation of approach,
    - indication of time spent
-}

import Exercise1
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Control.Monad

type Mutator = [Integer] -> Gen [Integer]
type ListProperty = [Integer] -> Integer -> Bool
type Fut = Integer -> [Integer]

-- List of tuple where the first element is a mutator and the second is a list of bool
-- The index of the element on the list is the same as the index of its Mutator on the mutator list
-- Each bool represents a property, with the link being made by both sharing the same index 
-- The value of the bool determines whether the propertie killed the mutant or not
mutatorBattle :: Fut -> Integer -> [ListProperty] -> [Mutator] -> [(Mutator, Gen [Bool])]
mutatorBattle fut input properties mutators = [(m, life) | m <- mutators, let life = mutate' m properties fut input]

propertiesStregth :: Fut -> Integer -> [ListProperty] -> [Mutator] -> IO Float
propertiesStregth fut input properties mutators = do
    let ms = filterM (snd >=> (return . not . null)) $ mutatorBattle fut input properties mutators
    mutatorStats <- generate ms

    -- Filter all the mutants to obtain only those who were killed by at least one property
    let mutantsKilled = filterM (snd >=> (return . not . and)) mutatorStats

    mk <- generate mutantsKilled
    -- Calculate the percentage of mutants killed
    return $ fromIntegral (length mk) / fromIntegral (length mutatorStats) * 100