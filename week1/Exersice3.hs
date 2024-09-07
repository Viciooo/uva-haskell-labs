module Lab0 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

data Shape = NoTriangle | Equilateral
 | Isosceles | Rectangular | Other deriving (Eq,Show)


noTraingle :: Integer -> Integer -> Integer -> Bool
noTraingle x y z = x + y <= z || x + z <= y || z + y <= x

equilateral :: Integer -> Integer -> Integer -> Bool
equilateral x y z = x == y && z == y && x == z

isosceles :: Integer -> Integer -> Integer -> Bool
isosceles x y z = x == y || x == z || z == y

rectangular :: Integer -> Integer -> Integer -> Bool
rectangular x y z = x*x + y*y == z*z ||  z*z + y*y == x*x ||  x*x + z*z == y*y

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
  | noTraingle x y z = NoTriangle
  | equilateral x y z = Equilateral
  | isosceles x y z = Isosceles
  | rectangular x y z = Rectangular
  | otherwise = Other

-- Time spent: 15 minutes
-- Question: Indicate how you tested or checked the correctness of the program
-- I would use the set of triples with correct answers and compare them with results