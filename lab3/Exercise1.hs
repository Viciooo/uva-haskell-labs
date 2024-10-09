-- Time Spent: 30 min
module Exercise1 where

import System.Random
import Test.QuickCheck
import SetOrd

{-Scratch
The values are limited to -100 to 100 to make it close to the quickCheck implementation
-}
randomSet :: IO (Set Int)
randomSet = do
    -- Choose a random value for the size of the list
    lenRand <- randomRIO (0, 100::Int)
    -- sequence is used to move from [IO Int] to IO[Int]
    list <- sequence [ randomRIO (-100, 100::Int) | _ <- [1..lenRand]]
    return $ list2set list


{-QuickCheck-}
-- https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck-Arbitrary.html#g:1
-- Creating an instante of Arbitrary for the type Set Int, using the minimum definition of arbitrary
instance Arbitrary (Set Int) where
    arbitrary = do 
        list <- arbitrary :: Gen [Int]
        return $ list2set list

randomSetQuickCheck :: Gen (Set Int)
randomSetQuickCheck = arbitrary