module Exercise6 where
import Test.QuickCheck
import Data.List
import Exercise3
import Exercise5

-- Checks if every element from the relation has its 
-- inverse in the relation
symClosProperty :: Rel Int -> Bool
symClosProperty xs = and [(x2,x1) `elem` xs | (x1, x2) <- xs]

-- Checks if there it can't be added a new relationship to the given relation 
trClosProperty :: Rel Int -> Bool
trClosProperty xs = length xs == length (nub $ xs ++ xs @@ xs)

-- Use quickcheck to generate the initial list of tuples
-- [(Int,Int)] is a type already implemented in QuickCheck 
testImplementation :: IO ()
testImplementation = do
    putStr "symClos Property="
    quickCheck $ symClosProperty.symClos

    putStr "trClos Property="
    quickCheck $ trClosProperty.trClos