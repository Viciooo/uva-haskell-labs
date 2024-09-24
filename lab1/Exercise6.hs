module Exercise6 where
import Lecture3

-- Uses distribute laws to create CNF format
-- It uses recursion to convert every disjunction to conjunction using distribute laws
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

-- arrowfree: 
-- P <=> Q
-- (P AND Q) OR (-P AND -Q)

-- 1. applaying arrfree function to get rid of arrows
-- 2. suing nnf to get rid of negation when possible
-- 3. using destribute laws to create final solution (CNF) 
cnf :: Form -> Form
cnf = distribute . nnf . arrowfree

-- form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))

a = arrowfree form1
b = nnf a

-- Converting form1 to CNF
cnfForm11 = cnf form1

-- Time Spent: 120 min