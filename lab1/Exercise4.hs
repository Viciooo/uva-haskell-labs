-- Time Spent: 45 min

module Exercise4 where
import Data.Maybe
import Data.List

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement [] _ = False
isDerangement _ [] = False
isDerangement (x:xs) (y:ys) = x /= y && elem x ys && isDerangement xs ys

deran :: Int -> [[Int]]
deran n = do
    let l = [0 .. n - 1]
    let p = permutations l
    filter (isDerangement l) p


{- Properties of isDeragement

A derangement is a list in which the original entities aren't in the same position 
isDerangement returns a bool, so the universe of outputs for this will be {True, False}
    - Property for when its not a derangment for a predefined case
    - Property to detect a derangement for a predefined case
    - Property to test against different list size
-}

isNotDerangement :: Bool
isNotDerangement = not $ isDerangement [1,2,3,4,5] [2,3,4,1,5]

isDerangementSuccess :: Bool
isDerangementSuccess = isDerangement [1,2,3,4,5] [5,1,2,3,4]

isNotDerangementSizes :: Bool
isNotDerangementSizes = not (isDerangement [1,2,3,4,5] [4,1,2,3]) && not (isDerangement [4,1,2,3] [1,2,3,4,5])

{- Order (ascending)
isNotDerangementSizes
isDerangementSuccess 
isNotDerangement
-}

{-
Yes, we can automate the testing of isDerangement by creating a random
list and then from that create a derangement for example by shifting
all numbers one position
-}