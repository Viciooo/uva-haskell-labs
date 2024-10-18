module Exercise1 where

{-
The IOLTS datatype allows, by definition, for the creation of IOLTS's that are not valid.
1. Make a list of factors that result in invalid IOLTS's.

2. Write a function  that returns true iff a given LTS is valid according to the definition 
given in the Tretmans paper with the following specification: validateLTS :: IOLTS -> Bool

3. Implement multiple concrete properties for this function (you will use QuickCheck to test 
them in a following Exercise)

Deliverables: list of factors, implementation, concise test report, indication of time spent.
-}

import LTS
import Data.List

{-1. Make a list of factors that result in invalid IOLTS's. 
An IOLTS is a modification of an LTS where the input and output labels are separated, i.e.
<Q,L,T,q0> => <Q,Li,Lo,T,q0>
Using the definition: type IOLTS = ([State], i[Label], o[Label], [LabeledTransition], State), the factors that
result in an invalide IOLTS are:
    - [State] is empty
    - The intersection of i[Label] and o[Label] is not empty
    - [LabeledTransition] contains at least one transition where either the label or the states are 
    not part of the corresponding list, i.e. i[Label], o[Label] and [State]
    - State is not part of [State]
-}

statesCheck :: [State] -> Bool
statesCheck states = not $ null states

labelsCheck :: [Label] -> [Label] -> Bool
labelsCheck l1 l2 = not $ any (`elem` l2) l1

transitionCheck :: [State] -> [Label] -> [Label] -> [LabeledTransition] -> Bool
transitionCheck states iLabels oLabels [] = True
transitionCheck states iLabels oLabels ((s0,l,s1):xs) = 
    (l `elem` iLabels || l `elem` oLabels) && 
    s0 `elem` states && s1 `elem` states && 
    transitionCheck states iLabels oLabels xs

initialStateCheck :: [State] -> State -> Bool
initialStateCheck states state0 = state0 `elem` states

-- Check if transitions are valid, i.e theres connection ?

validateLTS :: IOLTS -> Bool
validateLTS (states, iLabels, oLabels, transitions, state0) =
    statesCheck states &&
    labelsCheck iLabels oLabels &&
    transitionCheck states iLabels oLabels transitions &&
    initialStateCheck states state0

-- Properties: Properties defined in the usage
-- Test Report: Coverage of the properties