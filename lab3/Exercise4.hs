-- Time Spent: 120 min
module Exercise4 where
import Test.QuickCheck
import Data.List


type Rel a = [(a,a)]

-- A list of relantionships is considered serial if for every element x
-- of the domain, there is at leat one relation where xRy, where y is another
-- element of the domain. Due to this a reflexive set is also serial.
-- https://en.wikipedia.org/wiki/Relation_(mathematics) 
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial dom rel = and [ any (\r -> fst r == x) rel | x <- dom]

successProperty :: [Int] -> Bool
successProperty xs = do
    -- Remove repeated elements
    let cxs = nub xs
    -- The simplest possible linear set is one that is reflexive 
    let relList = [ (x,x) | x <- cxs]
    isSerial xs relList

failProperty :: [Int] -> Bool
failProperty xs = do
    -- Remove repeated elements
    let cxs = nub xs 
    -- Create a set like before, but remove one element so that is not serial
    let relList = [ (x,x) | x <- drop 1 cxs]
    -- Empty list need a special case for the property to work
    if null xs then isSerial xs relList else not $ isSerial xs relList

testImplementation :: IO ()
testImplementation = do
    putStr "Success Property="
    quickCheck successProperty

    putStr "Fail Property="
    quickCheck failProperty

{- R = {(x, y) | x ≡ y(mod n)}, n>0.
R is serial, because for every x, there will always be a y such that the mod
of n is the same for both. This is because the modulos represents the remainder
of a division, which is circular, i.e. between 1..n the value will go from 0..n-1
and once the number is >n, it will repeat the previous pattern, arrinving at 0
for every multiple of n. No more indications are given about the values x and y, then
we can assume that for every x in A, x is equal to its own module

How can you test whether R is serial? 
R can be tester if its serial by choosing a random positive integer for n,
on the list [1..n] such that each element has at least one

How can you prove that R is serial?
Given that neither x or y have any restrictions, for any domain A x mod n is always equal to
himself, i.e. for any domain A, R is reflexive. By definition of serial, if a set R is 
reflexive than is also serial.
-}
