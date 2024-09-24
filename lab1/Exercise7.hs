module Exercise7 where

import Lecture3 (Form(..))
import Test.QuickCheck
import SetOrd

-- USING INTs in a set is a bad idea! You should use something unique

-- Helper function to calculate the size of a set
sizeSet :: Set a -> Int
sizeSet (Set xs) = length xs

-- Function to extract sub-formulas of a formula
sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj fs) = foldr unionSet (Set [f]) (map sub fs)
sub f@(Dsj fs) = foldr unionSet (Set [f]) (map sub fs)
sub f@(Impl f1 f2) = unionSet (Set [f]) (unionSet (sub f1) (sub f2))
sub f@(Equiv f1 f2) = unionSet (Set [f]) (unionSet (sub f1) (sub f2))

-- Recursive function to count the number of sub-formulas
nsub :: Form -> Int
nsub (Prop _) = 1
nsub (Neg f) = 1 + nsub f
nsub (Cnj fs) = 1 + sum (map nsub fs)
nsub (Dsj fs) = 1 + sum (map nsub fs)
nsub (Impl f1 f2) = 1 + nsub f1 + nsub f2
nsub (Equiv f1 f2) = 1 + nsub f1 + nsub f2

-- QuickCheck property to verify that nsub matches the size of sub-formulas
prop_nsub_correct :: Form -> Property
prop_nsub_correct f =
  nsub f === sizeSet (sub f)

-- QuickCheck property to test the relation between sub-formulas, connectives, and atomic propositions
prop_subform_length_equals_ccount_plus_acount :: Form -> Property
prop_subform_length_equals_ccount_plus_acount f = 
  sizeSet (sub f) === ccount f + acount f

-- Function to count the number of connectives in a formula
ccount :: Form -> Int
ccount (Prop _) = 0
ccount (Neg f) = 1 + ccount f
ccount (Cnj fs) = 1 + sum (map ccount fs)
ccount (Dsj fs) = 1 + sum (map ccount fs)
ccount (Impl f1 f2) = 1 + ccount f1 + ccount f2
ccount (Equiv f1 f2) = 1 + ccount f1 + ccount f2

-- Function to count the number of atomic propositions in a formula
acount :: Form -> Int
acount (Prop _) = 1
acount (Neg f) = acount f
acount (Cnj fs) = sum (map acount fs)
acount (Dsj fs) = sum (map acount fs)
acount (Impl f1 f2) = acount f1 + acount f2
acount (Equiv f1 f2) = acount f1 + acount f2

-- Test for negations based on the induction hypothesis
prop_neg_subform_equals_ccount_acount :: Form -> Property
prop_neg_subform_equals_ccount_acount f =
  sizeSet (sub (Neg f)) === 1 + sizeSet (sub f) .&&.
  ccount (Neg f) === 1 + ccount f .&&.
  acount (Neg f) === acount f

-- ================== TESTS AND EXAMPLES ===================

instance Arbitrary Form where
  arbitrary = sized arbForm

-- Recursive generator with size limit to prevent infinite recursion
arbForm :: Int -> Gen Form
arbForm 0 = Prop <$> arbitrary  -- Base case: only propositions when depth is 0
arbForm n = oneof 
  [ Prop <$> arbitrary
  , Neg <$> arbForm (n `div` 2)
    -- Ensure at least two sub-formulas for Cnj and Dsj
  , Cnj <$> resize (n `div` 2) (listOf2 (arbForm (n `div` 2)))
  , Dsj <$> resize (n `div` 2) (listOf2 (arbForm (n `div` 2)))
  , Impl <$> arbForm (n `div` 2) <*> arbForm (n `div` 2)
  , Equiv <$> arbForm (n `div` 2) <*> arbForm (n `div` 2)
  ]

-- Helper function to generate at least 2 sub-formulas
listOf2 :: Gen a -> Gen [a]
listOf2 gen = do
  x <- gen
  xs <- listOf1 gen
  return (x:xs)

-- Example formulas for testing and understanding the functions
formula1 :: Form
formula1 = Impl (Prop 1) (Neg (Prop 2))

formula2 :: Form
formula2 = Cnj [Prop 1, Neg (Prop 2), Dsj [Prop 3, Prop 4]]

formula3 :: Form
formula3 = Equiv (Cnj [Prop 1, Prop 2]) (Dsj [Prop 3, Prop 4])

-- Function to print results for a given formula
printExample :: Form -> IO ()
printExample f = do
  putStrLn "============================="
  putStrLn $ "Formula: " ++ show f
  putStrLn $ "Sub-formulas: " ++ show (sizeSet (sub f))
  putStrLn $ "Number of sub-formulas: " ++ show (nsub f)
  putStrLn $ "Number of connectives (ccount): " ++ show (ccount f)
  putStrLn $ "Number of atoms (acount): " ++ show (acount f)
  putStrLn "============================="

main :: IO ()
main = do
  putStrLn "Example 1:"
  printExample formula1

  putStrLn "Example 2:"
  printExample formula2

  putStrLn "Example 3:"
  printExample formula3

  quickCheck prop_subform_length_equals_ccount_plus_acount
  quickCheck prop_neg_subform_equals_ccount_acount
  quickCheck prop_nsub_correct

-- This will fail for any formula with duplicate elements, because set will reduce them.
-- We should either use indexed Forms or uuids as 'name'

-- Time spent: 300min
