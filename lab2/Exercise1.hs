module Exercise1 where

import Mutation
import Test.QuickCheck


-- 1. addElements
-- Adds elements to the beginning and/or end of an output list
-- strengths: it checks situation when there are additional values in the list
-- weaknesses: it only adds values to the beginning or the end of the list so it is easy to catch for example by prop_tenElements

-- 2. removeElements
-- Removes 1 to (length - 1) elements from an output list
-- strengths: it checks situation when there are values in the list missing 
-- weaknesses: it only removes elemenst from the end of the list so it also is easy to catch

-- 3. Any list
-- strengths: can be helpful due to its randomness
-- weaknesses: it's suseptible to properties that check the length of the list, it should be length restricted for better performance

-- Outputs not coverd:
  -- empty list
  -- list with changed values
  -- infinite list
  -- reversed list
  -- shuffled list

-- Mutators covering the missing outputs:
-- 1. it changes the order of elements in the list (shuffle)
    -- strengths: checks want happens if oreder of elements is changed
    -- weaknesses: it doesn't modify, remove or add values from original list
-- 2. it changes one value in origianl list to a different one 
    -- strengths: checks what happens if one value is changed
    -- weaknesses: it doesn't remove or add elements to original output list
-- 3. it negates values from original list
    -- strengths: checks what happens if the numbers are negative
    -- weaknesses: it doesn't remove or add elements to original output list
-- 4. returns empty list
    -- strengths: checks edge case 
    -- weaknesses: easy to be catched
-- 5. it changes order of elements leaving only first and the last element untouched 
    -- strengths: checks what happens when order of elements form index 1 to list length - 2 is changed
    -- weaknesses: it doesn't modify, remove or add elements to original list
-- 6.  applying mathematical operation to every element in the list, for example substraction or multiplication
    -- strengths: checks what happens when all elements are modfied by the same function
    -- weaknesses:  it remove or add elements to original output list
-- 7. reverse output list
    -- strengths: checks what happens with the reversed list.
    -- weaknesses: it doesn't modify, remove or add elements to original output list and is easy to be killed
-- 8. inifinite list
    -- strengths: checks what happens when the output is inifinite list, it checks one of the edge cases
    -- weaknesses: computation will take a while ;)

-- 1.
orderChanged :: [Integer] -> Gen [Integer]
orderChanged xs = shuffle xs

-- 2.
changeOneValue :: [Integer] -> Gen [Integer]
changeOneValue [] = return [] 
changeOneValue xs = do
  let len = length xs
  index <- choose (0, len - 1)
  newValue <- arbitrary
  return [if i == index then newValue else x | (i, x) <- zip [0..] xs]

-- 3.
negateElements :: [Integer] -> Gen [Integer]
negateElements xs = return (map negate xs)

-- 4.
emptyList :: [Integer] -> Gen [Integer]
emptyList xs = return []

-- 5.
partyShuffle :: [Integer] -> Gen [Integer]
partyShuffle xs 
    | length xs < 3 = return xs
    | otherwise = do
        let first = head xs 
        let lastElem = last xs
        middle <- shuffle $ sublist xs
        return ([first] ++ middle ++ [lastElem])

sublist :: [Integer] -> [Integer]
sublist xs = take (length xs - 2) (drop 1 xs)

-- 6. one of the examples (increasing every element by 1)
incrementList :: [Integer] -> Gen [Integer]
incrementList xs = return (map (\x -> x + 1) xs)

-- 7. 
infinite :: [Integer] -> Gen [Integer]
infinite xs = return [1..]

-- Time spent: 60 min