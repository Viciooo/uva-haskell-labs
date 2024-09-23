
import Data.List

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = length xs == length ys && all (`elem` ys) xs 

-- Check if two lists are derangements of each other
isDerangement :: Eq a => [a] -> [a] -> Bool
-- we check if they are permutations and if no same element is in the same position
isDerangement xs ys = isPermutation xs ys && all (uncurry (/=)) (zip xs ys)


deran :: Int -> [[Int]]
deran n = filter (isDerangement [0..n-1]) $ permutations [0..n-1]

prop_listsHaveToBePermutation :: [Int] -> [Int] -> Bool
prop_listsHaveToBePermutation = isPermutation

prop_listsHaveToBeOfSameLength :: [Int] -> [Int] -> Bool
prop_listsHaveToBeOfSameLength xs ys = length xs == length ys

prop_rightHasToBeDerangement :: [Int] -> [Int] -> Bool
prop_rightHasToBeDerangement = isDerangement

data NamedFunction = NamedFunction {
    functionName :: String,
    function :: [Int] -> [Int] -> Bool
}


namedFunctions :: [NamedFunction]
-- Since functions don't have names in haskell we will have to walk around it
-- by introducing a mapping function
namedFunctions = [
    NamedFunction "same-length" prop_listsHaveToBeOfSameLength,
    NamedFunction "derangment" prop_rightHasToBeDerangement,
    NamedFunction "permutations" prop_listsHaveToBePermutation
    ]

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Check if property f is stronger than property g over a list of test cases
isStronger :: ([Int] -> [Int] -> Bool) -> ([Int] -> [Int] -> Bool) -> [[Int]] -> Bool
isStronger f g xs = all (\x -> f x x --> g x x) xs

compareStrength :: [[Int]] -> NamedFunction -> NamedFunction -> Ordering
compareStrength xs (NamedFunction _ f) (NamedFunction _ g)
  | isStronger f g xs = GT
  | isStronger g f xs = LT
  | otherwise = EQ

-- Function to order NamedFunctions by their strength
orderFunctionsByStrength :: [[Int]] -> [NamedFunction] -> [String]
orderFunctionsByStrength testCases functions =
    map functionName $ sortBy (compareStrength testCases) functions

main :: IO ()
main = do
    print $ isPermutation [1, 2, 3] [3, 2, 1] -- True
    print $ isPermutation [1, 2, 2] [2, 1, 2] -- True
    print $ isPermutation [1, 2, 3] [4, 5, 6] -- False
    print $ isPermutation [1, 2, 3] [1, 2]    -- False
    print $ isDerangement [1, 2, 3] [3, 1, 2] -- True
    print $ isDerangement [1, 2, 3] [1, 3, 2] -- False
    print $ deran 3 -- [[1,2,0],[2,0,1]]

        -- Test cases for ordering functions by strength
    let testCases = [[1, 2, 3,2,3,1,1], [0,2,3,1,1,2]]
    let orderedFunctionNames = orderFunctionsByStrength testCases namedFunctions
    print orderedFunctionNames -- Expected: Ordered list of function names

-- Time spent: 120min
