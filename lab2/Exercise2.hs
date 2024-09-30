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
-- Returns True if the mutators is still alive
battle :: Fut -> [ListProperty] -> [Mutator] -> IO Bool
battle fut properties mutators = do
    i <- generate $ choose (0, length mutators - 1)
    num <- generate randomInteger
    res <- generate $ mutate' (mutators !! i) properties fut num

    return $ and res

countSurvivors :: Integer -> Fut -> [ListProperty] -> [Mutator] -> IO Integer
countSurvivors 0 fut properties mutators = return 0

countSurvivors numMutants fut properties mutators = do
    battles <- sequence $ [battle fut properties mutators | b <- [1..numMutants]]

    return $ fromIntegral $ length $ filter not battles