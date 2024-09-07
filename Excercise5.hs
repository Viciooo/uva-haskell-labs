-- Implementing and testing ROT13 encoding
-- ROT13 is a single-letter substitution cipher that is used in online forums for hiding spoilers. See also www.rot13.com.
-- First, give a specification of ROT13.
-- Next, give a simple implementation of ROT13:
-- Finally, turn the specification into a series of QuickCheck testable properties, and use these to test your implementation.

import Data.Char
import Test.QuickCheck
import Test.QuickCheck.Gen (vectorOf)

rot13 :: Char -> Char
-- we check what kind of sign is it, we will modify it only if it's a letter
-- by using `ord c - ord 'a'` we get the index of the letter, then we move the index by 13,
-- `mod` is used to account for overflows, in those cases we add just the reminder.Applicative
-- finally we add the result to the code of starting letter and get actual letter using `chr`
rot13 c
    | c `elem` ['a'..'z'] = chr $ ord 'a' + (ord c - ord 'a' + 13) `mod` 26
    | c `elem` ['A'..'Z'] = chr $ ord 'A' + (ord c - ord 'A' + 13) `mod` 26
    | otherwise = c

rot13String :: String -> String
rot13String = map rot13

rot13CharReversability :: Property -- redundant, made it only for practice
rot13CharReversability = forAll arbitrary $ \c ->
    rot13 (rot13 c) == c

rot13Reversability :: Property
-- `vectorOf 10 arbitrary` gives 10 random chars
-- rot 13 has this awesome property that it's reversable, 
-- so we need to apply it twice and compare to original string to test it.
rot13Reversability = forAll (vectorOf 10 arbitrary) $ \s ->
    rot13String (rot13String s) == s

main :: IO ()
main = do
    quickCheck rot13CharReversability
    quickCheck rot13Reversability


