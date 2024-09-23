import Test.QuickCheck
-- To prove that there are 2^n subsets of a set with n elements for any n >= 0 we define a few functions.

powerset :: [a] -> [[a]]
powerset [] = [[]] -- special starting case
-- each element x can be either in the subset or not
-- xss are all subsets without x
-- map (x:) xss creates all subsets with x
powerset (x:xs) = xss ++ map (x:) xss 
    where xss = powerset xs -- applying recurrence

nonNegativeNumber :: Gen Integer
nonNegativeNumber = arbitrary `suchThat` (>= 0)

prop_powersetCardinality :: Property
prop_powersetCardinality = forAll nonNegativeNumber $ \n -> 
    length (powerset [1..n]) == 2 ^ n

nonNegativeNumberNotLargerThen20 :: Gen Integer
nonNegativeNumberNotLargerThen20 = choose (0, 20)

prop_powersetCardinalityForNnotLargerThen20 :: Property
-- out of curiosity I experimented with different ranges and found that only for n <= 20 
-- it was computationally feasible to test it on my machine.
-- This was quite surprising, but after a closer look it makes sense that the time complexity rises exposnetially.
prop_powersetCardinalityForNnotLargerThen20 = forAll nonNegativeNumberNotLargerThen20 $ \n -> 
    length (powerset [1..n]) == 2 ^ n


-- 1. Is the cardinality property of the powerset hard to test? If you find that it is, can you give a 
-- reason why?

-- Property of cardinality is easy to test codewise,
-- but computationally hard to test for bigger sets,
-- where n does not really have to be that big, due to O(2^n) time complexity.
-- Proving it is entirely different thing and I would use mathematical induction for that and not solve it in haskell.

-- 2. Give your thoughts on the following issue:  When you perform these tests, what are you 
-- testing actually? Are you checking a mathematical fact? Or are you testing whether 
-- powerset  satisfies a part of its specification? Or are you testing something else still?

-- In cardinality property test I'm only checking the equastion but not the correctness of the subsets.
-- I would say that testing it in haskell is not satisfactionary, 
-- because of small sample of numbers that we can actually test it on without hanging the machine.

main :: IO ()
main =
    do
    quickCheck prop_powersetCardinalityForNnotLargerThen20
    print $ powerset [1..3] 
    -- quickCheck prop_powersetCardinality -- hanging forever because of huge n's

-- Time Spent: 60min
