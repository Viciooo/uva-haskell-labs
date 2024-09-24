module Exercise6 where
import Lecture3

-- -- Flatten unnecessary nested disjunctions (ORs)
flattenOrs :: Form -> Form
flattenOrs (Dsj fs) = Dsj (concatMap flatten fs) -- Recursively flatten nested ORs
  where
    flatten (Dsj gs) = flattenForm (Dsj gs)  -- If it's an OR, flatten it
    flatten f        = [flattenOrs f]        -- Otherwise, just apply the flatten recursively

flattenOrs (Cnj fs) = Cnj (map flattenOrs fs) -- Recursively flatten ANDs
flattenOrs (Neg f)  = Neg (flattenOrs f)      -- Flatten negations recursively
flattenOrs (Prop x) = Prop x                  -- Base case: literals
flattenOrs f        = f                       -- Handle any other cases (Implications/Equivalences, etc.)

-- Flatten unnecessary nested conjunctions (ANDs)
flattenAnds :: Form -> Form
flattenAnds (Cnj fs) = Cnj (concatMap flatten fs) -- Recursively flatten nested ANDs
  where
    flatten (Cnj gs) = flattenForm2 (Cnj gs)  -- If it's an AND, flatten it
    flatten f        = [flattenAnds f]       -- Otherwise, just apply flatten recursively

flattenAnds (Dsj fs) = Dsj (map flattenAnds fs) -- Recursively flatten disjunctions (to maintain CNF structure)
flattenAnds (Neg f)  = Neg (flattenAnds f)      -- Flatten negations recursively
flattenAnds (Prop x) = Prop x                   -- Base case: literals
flattenAnds f        = f                        -- Handle any other cases (Implications/Equivalences, etc.)

-- Helper functions to flatten ORs and ANDs
flattenForm :: Form -> [Form]
flattenForm (Dsj fs) = concatMap flattenForm fs -- Merge nested disjunctions
flattenForm f        = [f]                      -- Keep non-OR elements as they are

flattenForm2 :: Form -> [Form]
flattenForm2 (Cnj fs) = concatMap flattenForm2 fs -- Merge nested conjunctions
flattenForm2 f        = [f]                      -- Keep non-AND elements as they are

-- 1. applaying arrfree function to get rid of arrows
-- 2. suing nnf to get rid of negation when possible
-- 3. using destribute laws to create final solution (CNF) 
distribute :: Form -> Form

-- f1 OR (f2 AND f2 AND ... fn)
-- fs -> f2..fn
-- map (\f -> distribute (Dsj [f1, f])) fs applies every literal from fs to dsj (f1 OR fx)
-- Cnj (map (\f -> distribute (Dsj [f1, f])) fs) applies Cnj to created Dsj 
distribute (Dsj [f1, Cnj fs]) = Cnj (map (\f -> distribute (Dsj [f1, f])) fs)
-- the same as above but inverted -- (f2 AND f2 AND ... fn) OR f1 
distribute (Dsj [Cnj fs, f2]) = Cnj (map (\f -> distribute (Dsj [f, f2])) fs)
distribute (Cnj fs) = Cnj (map distribute fs)
distribute (Dsj fs) = Dsj (map distribute fs)
distribute f = f


-- Check if a formula is a literal (Prop or Neg Prop)
isLiteral :: Form -> Bool
isLiteral (Prop _)   = True                   -- Atomic proposition
isLiteral (Neg (Prop _)) = True               -- Negation of an atomic proposition
isLiteral _ = False                           -- Anything else is not a literal

-- Check if a formula is a disjunction of literals
isDisjunctionOfLiterals :: Form -> Bool
isDisjunctionOfLiterals (Dsj fs) = all isLiteral fs  -- All elements in disjunction should be literals
isDisjunctionOfLiterals f = isLiteral f              -- A single literal is also considered valid

-- Check if a formula is in CNF form (conjunction of disjunctions of literals)
isCnf :: Form -> Bool
isCnf (Cnj fs) = all isDisjunctionOfLiterals fs      -- All elements in conjunction should be disjunctions of literals
isCnf f = isDisjunctionOfLiterals f                 -- A single disjunction of literals is also considered valid CNF

flatten :: Form -> Form
flatten = flattenAnds . flattenOrs

-- The cnf function applies arrowfree and nnf first, then repeatedly applies distribute until CNF is reached
cnf :: Form -> Form
cnf form = finalize (nnf . arrowfree $ form)

-- Recursive function that applies distribute until the form is in CNF
finalize :: Form -> Form
finalize form
    | isCnf (flatten form) = flatten form                -- If it's CNF, return the flattened form
    | otherwise            = finalize (distribute form)  -- Otherwise, apply distribute again

-- form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))

a = arrowfree form2
b = nnf a

-- Converting form1 to CNF
cnfForm11 = cnf form1

-- Time Spent: 120 min