--Time Spent: 10 min

module Exercise3 where
    
import Data.List
import Test.QuickCheck

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape

triangle a b c
    -- Test Triangle Inequality on the three sides to see if it can be considered a triangle
    | a + b < c || b + c < a || a + c < b = NoTriangle 
    -- Check if all sides are equal
    | a == b && b == c = Equilateral
    -- Check if any 2 sides can satisfy the Pithagoras Theorem
    | a^2 + b^2 == c^2 || a^2 + b^2 == c^2 || a^2 + b^2 == c^2 = Rectangular
    -- Check if 2 sides are equal
    | a == b || b == c || a == c = Isosceles 
    | otherwise = Other

{-
The Triangles are tested in a specific order, so that assumptions can be made about it's nature.
First we test whether or not the three sides can be part of a triangle, so by passing this we can assume
that other guards below are dealing with triangles. 
We test Isosceles after both Equilateral and Rectangular, because due to it's weak specifity, i.e.,
two sides have the same length, it could overlap with a more specific category of triangles that also
satisfy this.

The only sure way to test this is with a predefined data set of three values which are both Triangles and Not Triangles
and test the classification result given by the function with the expected classification that comes from the data.
-}

    