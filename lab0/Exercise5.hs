--Time Spent: 15 min
module Exercise5 where

import Data.Char
import Data.List
import Test.QuickCheck

phraseGen :: Gen [Char]
phraseGen = arbitrary

{-
    Specification:
        -All letters are transformed in the next 13th letter.
        -Applying ROT13 on ROT13 will give the original information
-}

rot13 :: [Char] -> [Char]
rot13 = map (
    \x ->
        if isAsciiUpper x then chr (65 + mod (ord x - 65 + 13) 26)
        else if isAsciiLower x then chr (97 + mod (ord x - 97 + 13) 26)
        else x)

{-
Receive a String and an ciphered String and return whether or not the ciphered String is equal to the string
Check wheter the difference between each character is 13
-}
rot13Eval :: [Char] -> [Char] -> Bool
rot13Eval [] [] = True
rot13Eval (x:xs) [] = False
rot13Eval [] (y:ys) = False
rot13Eval (x:xs) (y:ys) = 
    (if ord y > ord x then ord y - ord x == 13
    else if ord x > ord y then abs (ord x - ord y) == 13
    else ord x == ord y) && rot13Eval xs ys

-- Test correct cipher
correctCipherProperty :: Property
correctCipherProperty = forAll phraseGen $ \str -> rot13Eval str $ rot13 str 

-- Test deciphering by ciphering acipher 
doubleCipherProperty :: Property
doubleCipherProperty = forAll phraseGen $ \str -> str `isSubsequenceOf` rot13 (rot13 str)

main :: IO ()
main = do
    print "Cipher?"
    quickCheck correctCipherProperty
    
    print "Double Cipher?"
    quickCheck doubleCipherProperty
    





