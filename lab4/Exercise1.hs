
module Exercise1 where
import LTS
import Data.List

-- The IOLTS datatype allows, by definition, for the creation of IOLTS's that are not valid. 
-- 1. Make a list of factors that result in invalid IOLTS's. 
-- 2. Write a function  that returns true iff a given LTS is valid according to the definition 
-- given in the Tretmans paper with the following specification:
-- validateLTS :: IOLTS -> Bool
-- 3. Implement multiple concrete properties for this function (you will use QuickCheck to test 
-- them in a following Exercise)
-- Deliverables: list of factors, implementation, concise test report, indication of time spent.

-- Time Spent: 120 min

-- [1] Factors that make an IOLTS invalid:

-- 1. The set of states is an empty set
-- 2. Initial state is not in the set of states
-- 3. IOLTS tuple doesn't have 5 values, or they are incompatible with types - Haskell will detect it and throw an error, so no need to check for it
-- 4. At least 1 element of the input and output sets is the same
-- 5. Factor tau is in the set of inputs
-- 6. Either the input or output set is not countable
-- 7. Transition relation does not satisfy the cartesian product constraint

-- Helper function to check for the subset of the transitions
createCartesian :: [State] -> [Label] -> [(State, Label, State)]
createCartesian setState inputValues =  [(preState, transitionLabel, afterState) | preState <- setState, transitionLabel <- inputValues ,afterState <- setState ]

-- Helper function to check if the elements of a set are countable
isCountable :: [Label] -> Bool
isCountable inputOrOutput = True

-- [2] Function to validate the IOLTS
validateLTS :: IOLTS -> Bool
validateLTS ([], _, _, _, _) = False -- 1. If your initial set is empty, it is not an LTS
validateLTS (setState, inputValues, outputValues, labeledTransitions, initialState)
    | initialState `notElem` setState = False -- 2. initial state should be in the set of states
    | not (null (inputValues `intersect` outputValues)) = False -- 4. check if they intersect
    | tau `elem` inputValues = False -- 5. tau should not be in input values
    | not(isCountable inputValues) = False -- 6. check if input is countable
    | not(isCountable outputValues) = False -- 6. check if output is countable
    | not (all (`elem` createCartesian setState (inputValues ++ [tau] ++ [delta] ++ outputValues)) labeledTransitions) = False -- 7. Check if the transition relation follows the cartesian product constraint
    | otherwise = True

-- [3] Properties for testing if an IOLTS is valid

-- Factor 1
prop_initialSetNotEmpty :: IOLTS -> Bool
prop_initialSetNotEmpty (setOfStates, _, _, _, _) = not (null setOfStates)

-- Factor 2
prop_initialStateInStateSet :: IOLTS -> Bool
prop_initialStateInStateSet (stateSet, _, _, _, initialValue) = initialValue `elem` stateSet

-- Factor 4
prop_inputOutputIntersectionEmpty :: IOLTS -> Bool
prop_inputOutputIntersectionEmpty (_, inputValues, outputValues, _, _) = null (inputValues `intersect` outputValues)

-- Factor 5
prop_tauNotInInputSet :: IOLTS -> Bool
prop_tauNotInInputSet (_, inputValues, _, _, _) = tau `notElem` inputValues

-- Factor 6
prop_bothInputAndOutputCountable :: IOLTS -> Bool
prop_bothInputAndOutputCountable (_, inputValues, outputValues, _, _) =
    isCountable inputValues && isCountable outputValues

-- Factor 7
prop_cartesianRelationInTransition :: IOLTS -> Bool
prop_cartesianRelationInTransition (stateSet, inputValues, outputValues, labeledTransitions, _) =
    all (`elem` createCartesian stateSet (inputValues ++ [tau] ++ [delta] ++ outputValues)) labeledTransitions

-- Test Report: Validated against the examples in the LTS module and they all return True

main :: IO ()
main =  do
        let ltsList = [
                                counterImpl,counterModel,
                                coffeeImpl1, coffeeModel1,
                                coffeeImpl2, coffeeModel2,
                                coffeeImpl3,coffeeModel3,
                                coffeeImpl4,coffeeModel4,
                                coffeeImpl5,coffeeModel5,
                                coffeeImpl6,coffeeModel6,
                                tretmanK2,tretmanK3,
                                tretmanI1,tretmanI2,tretmanI3,tretmanI4,
                                tretmanS1,tretmanS2,tretmanS3,tretmanS4,
                                tretmanR1,tretmanR2]
        print (all validateLTS ltsList) 
        -- if all are valid according to validateLTS, then the test returns True
