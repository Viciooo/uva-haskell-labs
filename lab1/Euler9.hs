module Euler9 where

{-
With f => a + b + c = 1000
With this we can then define c = 1000 - a - b so that a^2 + b^2 = (1000-a-b)^2
f limits the possible values of 'a','b' up to 500
-}

{-
Go over all the possible combinations of a,b
Because sum is associative, we define a range of values for 'a' to be 
[1..500] and for 'b' to be [x..500] so that combinations of the sum are
tested only once
Testing: With the return values, calculate c using the pythagoras theorem and then 
check if a + b + c = 1000
-}
computeAB :: (Integer, Integer)
computeAB = head [ (x,y) | x <- [1..500], y <- [x..500], x^2 + y^2 == (1000-x-y)^2]

euler9 :: Integer
euler9 = do
    let ab = computeAB
    -- Find C from the pythagoras theorem
    let c = floor $ sqrt $ fromIntegral $ (fst ab)^2 + (snd ab)^2
    c * (fst ab) * (snd ab)