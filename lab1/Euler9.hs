module Euler9 where

{-
With f => a + b + c = 1000
With this we can then define c = 1000 - a - b so that a^2 + b^2 = (1000-a-b)^2
f limits the possible values of a,b,c up to 998, 
assuming that there a case such that 1 + 1 + 998 = 1000
-}

{-
Go over all the possible combinations of a,b
Because sum is associative, we define a range of values for a to be 
[1..998] and for b to be [x..998] so that combinations of the sum are
tested only once
-}
computeAB :: [(Integer, Integer)]
computeAB = [ (x,y) | x <- [1..998], y <- [x..998], x^2 + y^2 == (1000-x-y)^2]

euler9 :: Integer
euler9 = do
    let ab = head computeAB
    -- Find C from the pythagora theorem
    let c = floor $ sqrt $ fromIntegral $ (fst ab)^2 + (snd ab)^2
    c * (fst ab) * (snd ab)

{- Testing Description

-}