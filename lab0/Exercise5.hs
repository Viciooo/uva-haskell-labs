import Data.Char (ord)
import Data.Char (chr)
import Data.Char (isUpper, isLower)
import Test.QuickCheck
-- Specification: 
-- Program is channing every letter in alphabet for the letter 13 places further

charToUnicode :: Char -> Int
charToUnicode = ord

translateCapital :: Char -> Char
translateCapital c = chr (rem ((ord c) - 65 + 13) 26 + 65)

translateLowerCase :: Char -> Char
translateLowerCase c = chr (rem ((ord c) - 97 + 13) 26 + 97)

translate :: Char -> Char
translate c
  | isUpper c = translateCapital c
  | isLower c = translateLowerCase c
  | otherwise = c

rot13 :: [Char] -> [Char]
rot13 xs = [translate x | x <- xs]

genChar :: Gen Char
genChar = arbitrary :: Gen Char

reversalSymmetry :: Char -> Bool
reversalSymmetry c = c == translate (translate c)

reversalSymmetryProperty :: Property
reversalSymmetryProperty = forAll genChar $ \c -> reversalSymmetry c

-- genCharArray :: Gen [Char]
-- genCharArray = listOf arbitrary

genCharArray :: Gen [Char]
genCharArray =  arbitrary

reversalArraySymmetry :: [Char] -> Bool
reversalArraySymmetry xs = xs == rot13 (rot13 xs)

reversalArraySymmetryProperty :: Property
reversalArraySymmetryProperty = forAll genCharArray $ \c -> reversalArraySymmetry c

-- maybe the same number of unigue values

main :: IO ()
main =  do
    quickCheck reversalSymmetryProperty
    quickCheck reversalArraySymmetryProperty
