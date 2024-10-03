-- Time Spent: 120 min
module Exercise2 where

import Exercise1
import SetOrd
import Test.QuickCheck

-- import Control.Applicative (Alternative(some))

exposeSet :: Set a -> [a]
exposeSet (Set s) = s

-- Return a set with Ord a => Set a -> Set a -> Set a elements that are present in booth sets
setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set s1) (Set s2) = list2set $ filter (`elem` s2) s1

-- Union is the merger of the two sets
setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set s1) (Set s2) = list2set (s1 ++ s2)

-- The difference between two sets is the elements of the union that don't belong to the intersection
setDifference :: Ord a => Set a -> Set a -> Set a
setDifference s1 s2 = do
    let intersectionList = exposeSet $ setIntersection s1 s2
    let unionList = exposeSet $ setUnion s1 s2

    list2set $ filter (`notElem` intersectionList) unionList


-- Properties

-- Check if all elements of the intersection Set are part of both original Sets
-- It only guarantees that the first set is an intersection of the 2 given, not that is a complete intersection
intersectionProperty :: Ord a => Set a -> Set a -> Set a -> Bool
intersectionProperty (Set sInt) (Set s1) (Set s2) =  and [ x `elem` s1 && x `elem` s2  | x <- sInt]

-- Check if all elements of the original Sets are part of the union Set
unionProperty :: Ord a => Set a -> Set a -> Set a -> Bool
unionProperty (Set sUn) (Set s1) (Set s2) =  and ( [ x `elem` sUn | x <- s1] ++ [ x `elem` sUn | x <- s2])

-- Check if all elements of the difference Set are part of one and only one of the original Sets
differenceProperty :: Ord a => Set a -> Set a -> Set a -> Bool
differenceProperty (Set sDiff) (Set s1) (Set s2) =  and [ (x `elem` s1 || x `elem` s2) && not (x `elem` s1 && x `elem` s2)  | x <- sDiff]

--Tests
testIntersection :: (Set Int, Set Int) -> Bool
testIntersection (s1,s2) = do
    let s3 = setIntersection s1 s2
    intersectionProperty s3 s1 s2

testUnion:: (Set Int, Set Int) -> Bool
testUnion (s1,s2)  = do
    let s3 = setUnion s1 s2
    unionProperty s3 s1 s2

testDifference :: (Set Int, Set Int) -> Bool
testDifference (s1,s2)  = do
    let s3 = setDifference s1 s2
    differenceProperty s3 s1 s2
        

quicChec :: ((Set Int, Set Int) -> Bool) -> Int -> IO Bool
quicChec fun 0 = return True
quicChec fun n = do
    s1 <- randomSet
    s2 <- randomSet
    
    next <- quicChec fun (n-1)
        
    return $ fun (s1,s2) && next

testImplementation :: IO ()
testImplementation = do
    putStrLn "Using Scratch Generator"
    putStr "Intersection="
    t1 <- quicChec testIntersection 100
    if t1 then putStrLn "Pass" else putStrLn "Fail"
    putStr "Union="
    t2 <- quicChec testUnion 100
    if t2 then putStrLn "Pass" else putStrLn "Fail"
    putStr "Difference="
    t3 <- quicChec testDifference 100
    if t3 then putStrLn "Pass" else putStrLn "Fail"

    -- Use the instance Arbitrary predefined previously
    putStrLn "Using QuickCheck Generator"
    putStr "Intersection="
    quickCheck testIntersection
    putStr "Union="
    quickCheck testUnion
    putStr "Difference="
    quickCheck testDifference