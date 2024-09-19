-- 2h
module Exercise6 where

import Lecture3

flat :: Form -> Form
-- Dsj [Form, Form]
flat p@(Dsj f) =  nnf (Neg p)
flat p@(Cnj f) = nnf (Neg p)

cnf :: Form -> Form
cnf f = flat $ nnf $ arrowfree f

