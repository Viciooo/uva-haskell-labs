module Exercise2 where

import Data.Char
import Data.List
import System.Random
import Test.QuickCheck

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
  p <- getStdRandom random
  ps <- probs (n - 1)
  return (p : ps)

toQuartile :: Float -> Int
toQuartile x = floor(x * 4) -- this is a clever little trick to avoid going case by case
-- since 1 is not in the possible results, there are no edge cases

-- we could also use something like this:

-- mapToQuartile :: Float -> Int
-- mapToQuartile x = case () of
--   _
--     | x >= 0 && x < 0.25 -> 1
--     | x >= 0.25 && x < 0.5 -> 2
--     | x >= 0.5 && x < 0.75 -> 3
--     | x >= 0.75 && x <= 1 -> 4
--     | otherwise -> error "Value out of range"

mapToQuartile :: [Float] -> [Int]
mapToQuartile = map toQuartile

reduceToNumberPerQuartile :: [Int] -> (Int, Int, Int, Int)
reduceToNumberPerQuartile = foldl' updateCount (0, 0, 0, 0)
-- Using foldl' because it's more efficient
-- https://wiki.haskell.org/Foldr_Foldl_Foldl%27
  where
    updateCount (q0, q1, q2, q3) x
      | x == 0    = (q0 + 1, q1, q2, q3)
      | x == 1    = (q0, q1 + 1, q2, q3)
      | x == 2    = (q0, q1, q2 + 1, q3)
      | x == 3    = (q0, q1, q2, q3 + 1)
      | otherwise = error "Value out of range"
-- we go case by case counting the mapped values and saving them to a result tuple

closeToQuarter :: (Int, Int) -> Bool
closeToQuarter (groupSamples, numberOfSamples) =
  abs (groupSamples - (numberOfSamples `div` 4)) < round (fromIntegral numberOfSamples * 0.01)
-- for us "roughly" eqaul proportions are those that are within 1% of the expected value 


solution :: ((Int, Int, Int, Int), Int) -> Bool
solution ((q1, q2, q3, q4), numberOfSamples) =
  all (\q -> closeToQuarter (q, numberOfSamples)) [q1, q2, q3, q4]
-- we check if all the quartiles are within 1% of the expected value and return a sum of bools

main :: IO ()
main = do
  let numberOfSamples = 10000
  ps <- probs numberOfSamples
  let quartileCounts = reduceToNumberPerQuartile $ mapToQuartile ps
  -- firstly we map the values to quartiles and then count the amount of values in each quartile
  let result = solution (quartileCounts, numberOfSamples)

  print result 

-- Comments:
-- Firstly we map the values generated to quartiles and then count amount of values in each quartile.
-- If each quartile has no more then 1% difference from the expected value, we consider the solution correct.
-- Time spent: 120min
