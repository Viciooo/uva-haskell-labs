module Exercise6 where
import Lecture3

-- According to associative law: (q OR p) OR z = q OR (p OR z), this work the same for AND
-- Functions associativeOrs and associativeAnd use this law to get rid of unecessery brackets

-- It works depending what the input is:
-- if it is a literal it just returns it,
-- if it is negation or conjuntion it tries to remove brackets from clauses inside of it
-- if it is a disjuction it use recursion to get to every disjuction inside of it to extarct literals from it and put them inside one disjuction
-- note: we dont have to take into consideration the implication or equivalence becuse this fucntion will be run on formula with removed arrows

associativeOrs :: Form -> Form
associativeOrs (Dsj fs) = Dsj (concatMap helperOrs fs) -- removes brackets from clauses inside main clause using helper function
associativeOrs (Cnj fs) = Cnj (map associativeOrs fs) -- recursivly removes brackets from clauses inside conjunction
associativeOrs (Neg f)  = Neg (associativeOrs f)   -- recursivly removes brackets from clauses inside negation
associativeOrs (Prop x) = Prop x -- if the input is a literal, function just returns it

-- Helper functions to flatten ORs
helperOrs :: Form -> [Form]
helperOrs (Dsj fs) = concatMap helperOrs fs -- recursivly enters next disjutions
helperOrs f        = [f]  -- if the input is a literal it return one element list with it (so then concatMap can create on list of all literals)


-- the same scheme as for associativeOrs but in this case function tries to get rid of brackets bewteen Ands
-- note: in this case we dont't have into cosinderation case of disjunction because 
-- if the previously used funtions on input formula works well there shoudn't be any conjunstions inside disjuction
associativeAnds :: Form -> Form
associativeAnds (Cnj fs) = Cnj (concatMap helperAnds fs)  -- removes brackets from conjunctions inside main conjunction using helper function
associativeAnds (Dsj fs) = Dsj (map associativeAnds fs) -- recursivly removes brackets from conjunctions inside conjunction
associativeAnds (Neg f)  = Neg (associativeAnds f)  -- recursivly removes brackets from conjunctions inside negation
associativeAnds (Prop x) = Prop x  -- if the input is a literal, function just returns it

-- Helper functions to flatten ANDs
helperAnds :: Form -> [Form]
helperAnds (Cnj fs) = concatMap helperAnds fs 
helperAnds f        = [f]

-- Distributive laws describes interaction between conjunctions and disjunctions
-- We use this laws in function distribute to remove disjuction between conjuctions
-- note: this method won't work if the conjutions and dijaction are mixed to deep, so in the final function we will apply this method to the formula mulitple times
distribute :: Form -> Form
distribute (Dsj [f1, Cnj fs]) = Cnj (map (\f -> distribute (Dsj [f1, f])) fs) -- it applies disjuction to every element in list inside conjuction and the apply conjunction on all the results
distribute (Dsj [Cnj fs, f2]) = Cnj (map (\f -> distribute (Dsj [f, f2])) fs) -- it does the same as case above but when when first element of disjuction is conjuction (ex. (f2 AND f3 AND ... fn) OR f1)
distribute (Cnj fs) = Cnj (map distribute fs) -- recurisvly applies function to elements inside conjuction (if the elements don't match pattern above)
distribute (Dsj fs) = Dsj (map distribute fs) -- recurisvly applies function to elements inside disjuctions (if the elements don't match pattern above)
distribute f = f -- in other case (being a literal) it returns the value


-- This method chech if provided formula is indeed a CNF form
-- formula provided as input shoud be without uncessery brackets so it should be in formula *(fs) ex. *(+(p q) +(-p q)) or in formula without conjunctions at all ex. +(p q)

isCnf :: Form -> Bool
isCnf (Cnj fs) = all isDisjunction fs   -- check the first case when formula start with conjuction (it checks if every element in conjuction is a disjuction or literal)
isCnf f = isDisjunction f  -- second case, without conjuction in input formula (it check if all elements are disjuctions or literals)

-- Check if a formula is a disjunction of literals
isDisjunction :: Form -> Bool
isDisjunction (Dsj fs) = all isLiteral fs  -- if input is disjunction it check if all elements inside are literals
isDisjunction f = isLiteral f              -- if it is not a disjuntion it check if it is a literal

-- This function check if input is a literal or negation of literal
isLiteral :: Form -> Bool
isLiteral (Prop _)   = True
isLiteral (Neg (Prop _)) = True
isLiteral _ = False

-- Function removing uneccesery bracket between ORs and ANDs (it combines functions associativeAnds and associativeOrs)
flatten :: Form -> Form
flatten = associativeAnds . associativeOrs

-- This function translate formula to CNF form
cnf :: Form -> Form
cnf form = checkCorrectnes (nnf . arrowfree $ form)

-- Recursive function that applies distribute function until flattened form is correct (is CNF form)
checkCorrectnes :: Form -> Form
checkCorrectnes form
    | isCnf (flatten form) = flatten form                -- If it is CNF than return flatten version of it
    | otherwise            = checkCorrectnes (distribute form)  -- if it is not CNF form apply distribute function one more time


-- cnf work flow:
-- 1. Apply arrowfree to get rid of arrows (implications and equivalences) in formula
-- 1. Apply nnf to get rid of uneccesery negation
-- 2. Apply distribute function until its flattened version is a CNF form
-- 3. Apply flatten function to result

-- Time Spent: 420 min