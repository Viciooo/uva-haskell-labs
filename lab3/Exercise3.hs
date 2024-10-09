-- Time Spent: 10 min
module Exercise3 where
import Data.List

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos xs = sort $ nub $ xs ++ [ (x2,x1) | (x1, x2) <- xs ]