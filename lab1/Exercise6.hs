module Exercise6 where
import Lecture3

-- According to associative law: (q OR p) OR z = q OR (p OR z), this works the same for AND
-- Functions associativeOrs and associativeAnd use this law to get rid of unnecessary brackets

-- It works depending on what the input is:
-- if it is a literal it just returns it,
-- if it is negation or conjunction it tries to remove brackets from clauses inside of it
-- if it is a disjunction it uses recursion to get to every disjunction inside of it to extract literals from it and put them inside one disjunction
-- note: we don't have to take into consideration the implication or equivalence because this fucntion will be run on a formula with removed arrows

associativeOrs :: Form -> Form
associativeOrs (Dsj fs) = Dsj (concatMap helperOrs fs) -- removes brackets from clauses inside main clause using helper function
associativeOrs (Cnj fs) = Cnj (map associativeOrs fs) -- recursively removes brackets from clauses inside conjunction
associativeOrs (Neg f)  = Neg (associativeOrs f)   -- recursively removes brackets from clauses inside negation
associativeOrs (Prop x) = Prop x -- if the input is a literal, the function just returns it

-- Helper functions to flatten ORs
helperOrs :: Form -> [Form]
helperOrs (Dsj fs) = concatMap helperOrs fs -- recursively enters next disjunctions
helperOrs f        = [f]  -- if the input is a literal it returns one element list with it (so then concatMap can create a list of all literals)


-- the same scheme as for associativeOrs but in this case, the function tries to get rid of brackets between Ands
associativeAnds :: Form -> Form
associativeAnds (Cnj fs) = Cnj (concatMap helperAnds fs)  -- removes brackets from conjunctions inside the main conjunction using the helper function
associativeAnds (Dsj fs) = Dsj (map associativeAnds fs) -- recursively removes brackets from conjunctions inside conjunction
associativeAnds (Neg f)  = Neg (associativeAnds f)  -- recursively removes brackets from conjunctions inside negation
associativeAnds (Prop x) = Prop x  -- if the input is a literal, the function just returns it

-- Helper functions to flatten ANDs
helperAnds :: Form -> [Form]
helperAnds (Cnj fs) = concatMap helperAnds fs 
helperAnds f        = [f]

-- Distributive laws describe interaction between conjunctions and disjunctions
-- We use this law in function distribution to remove disjunction between conjunctions
-- note: this method won't work if the conjunctions and disjunction are mixed too deep, so in the final function we will apply this method to the formula multiple times
distribute :: Form -> Form
distribute (Dsj [f1, Cnj fs]) = Cnj (map (\f -> distribute (Dsj [f1, f])) fs) -- it applies disjunction to every element in the list inside conjunction and the apply conjunction on all the results
distribute (Dsj [Cnj fs, f2]) = Cnj (map (\f -> distribute (Dsj [f, f2])) fs) -- it does the same as the case above but when the first element of disjunction is conjunction (ex. (f2 AND f3 AND ... fn) OR f1)
distribute (Cnj fs) = Cnj (map distribute fs) -- recursively applies function to elements inside conjunction (if the elements don't match the attern above)
distribute (Dsj fs) = Dsj (map distribute fs) -- recursively applies function to elements inside disjunctions (if the elements don't match the pattern above)
distribute f = f -- in other case (being a literal) it returns the value


-- This method checks if provided formula is indeed a CNF form
-- formula provided as input should be without unnecessary brackets so it should be in the form *(fs) ex. *(+(p q) +(-p q)) or in form without conjunctions at all ex. +(p q)

isCnf :: Form -> Bool
isCnf (Cnj fs) = all isDisjunction fs   -- check the first case when the formula starts with conjunction (it checks if every element in conjunction is a disjunction or literal)
isCnf f = isDisjunction f  -- second case, without conjunction in input formula (it checks if all elements are disjunctions or literals)

-- Check if a formula is a disjunction of literals
isDisjunction :: Form -> Bool
isDisjunction (Dsj fs) = all isLiteral fs  -- if the input is disjunction it checks if all elements inside are literals
isDisjunction f = isLiteral f              -- if it is not a disjunction it check if it is a literal

-- This function checks if input is a literal or negation of a literal
isLiteral :: Form -> Bool
isLiteral (Prop _)   = True
isLiteral (Neg (Prop _)) = True
isLiteral _ = False

-- Function removing unnecessary bracket between ORs and ANDs (it combines functions associativeAnds and associativeOrs)
flatten :: Form -> Form
flatten = associativeAnds . associativeOrs

-- This function translate the formula to CNF form
cnf :: Form -> Form
cnf form = checkCorrectnes (nnf . arrowfree $ form)

-- Recursive function that applies distribute function until the flattened form is correct (is CNF form)
checkCorrectnes :: Form -> Form
checkCorrectnes form
    | isCnf (flatten form) = flatten form                -- If it is CNF then return flatten version of it
    | otherwise            = checkCorrectnes (distribute form)  -- if it is not CNF form apply distribute function one more time


-- cnf work flow:
-- 1. Apply arrowfree to get rid of arrows (implications and equivalences) in the formula
-- 1. Apply nnf to get rid of unnecessary negation
-- 2. Apply distribute function until its flattened version is a CNF form
-- 3. Apply flatten function to the result

-- Time Spent: 420 min
