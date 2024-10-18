--Time Spent: 120 min
module Exercise2 where

import System.Random ()
import Test.QuickCheck
import LTS
import Exercise1


-- 1. Implement at least one random generator  ltsGen :: Gen IOLTS  for Labelled Transition Systems.

-- Generate a random transition with initial state in index 'i'
randomTransition :: Int -> [State] -> [Label] -> [Label] -> Gen LabeledTransition
randomTransition i states labelsI labelsU = do
    -- Get the initial state of the transitions
    let state = states !! i

    -- Choose a random next state
    iNextState <- choose (0, length states - 1 ::Int)
    -- Choose a random label
    iNextLabel <- choose (0, length labelsI - 1 ::Int)
    -- Choose whether the label is I, U or tau
    labelType <- choose (0, 2 ::Int)

    let label
          | labelType == 0 = labelsI !! iNextLabel
          | labelType == 1 = labelsU !! iNextLabel
          | otherwise = tau
          
    let nextState = states !! iNextState

    return (state, label, nextState)

-- Generate a random number of transitions for the state in index 'i'
randomTransitions :: Int -> [State] -> [Label] -> [Label] -> Gen [LabeledTransition]
randomTransitions i states labelsI labelsU = do
    -- Choose the number of connections for the current state
    -- 5 is an arbitrary small number
    nConnections <- choose (1, 5::Int)

    sequence [ randomTransition i states labelsI labelsU | _ <- [1..nConnections]]


-- Generate random transitions for each state
genTransitions :: [State] -> [Label] -> [Label] -> Gen [LabeledTransition]
genTransitions [] labelsI labelsU = return []
genTransitions states labelsI labelsU = sequence ([randomTransitions i states labelsI labelsU | i <- [0 .. length states - 1]]) >>= \ll -> return $ concat ll


ltsGen :: Gen IOLTS
ltsGen = do
    -- Select a random number of states
    nStates <- choose (2, 10 ::Integer)
    let states = [0.. (nStates - 1)]

    -- Select a random number of labels
    -- Initially the labels have the same size, but by creating the transitions randomly
    -- the final number for each one is not always the sames
    nLabels <- choose (1, 10::Int)

    let labelsI = [ "?" ++  show x | x <- [1..nLabels]]
    let labelsU = [ "!" ++  show x | x <- [11.. 10 + nLabels]]

    transitions <- genTransitions states labelsI labelsU

    return $ createIOLTS transitions

--3. Use your generator(s) to test the properties implemented in the previous exercise.
testPropertiesGenerator :: IO ()
testPropertiesGenerator =  do
    iolts <- generate ltsGen
    print iolts

    putStr "The set of states is not an empty set = "
    print $ prop_initialSetNotEmpty iolts
    putStr "Initial state is in the set of states = "
    print $ prop_initialStateInStateSet iolts
    putStr "No elements in commont in the input and output sets = "
    print $ prop_inputOutputIntersectionEmpty iolts
    putStr "Factor tau is not in the set of inputs = "
    print $ prop_tauNotInInputSet iolts
    putStr "The input and output set are countable = "
    print $ prop_bothInputAndOutputCountable iolts
    putStr "Transition relation satisfies the cartesian product constraint = "    
    print $ prop_cartesianRelationInTransition iolts

quickCheckTest = do
    quickCheck $ forAll ltsGen validateLTS