module Exercise4 where

import LTS
import Data.List
import Test.QuickCheck
import Data.Traversable (for)

-- This fucnction adds all states to final result that are reachable by tau transitions
addTauTransitions :: [State] -> [LabeledTransition] -> [State]
addTauTransitions resultStates transitions = do
    let tauStates = [y | (x, label, y) <- transitions, label == tau, x `elem` resultStates]
    resultStates ++ filter (`notElem` resultStates) tauStates

-- This function deletes all delta from input traces cause they wont change the result
-- (they are always in LabeledTransition like (x, delta, x) so they cannot result in adding new state to result)
removeDeltas :: Trace -> Trace
removeDeltas = filter (/= delta)

-- This function returns states that are reachable from current state by given label
nextState :: State -> Label -> [LabeledTransition] -> [State]
nextState currentState givenLabel transitions =
    [y | (x, label, y) <- transitions, x == currentState, label == givenLabel]

-- This function use recursion to find all final states (without does that are reachable by tau). 
-- For first state and for label first in given trace if finds all possible states that can be reachable 
-- and for every found state it runs the function again with founed state and next lable in trace
after' :: IOLTS -> Trace -> [State]
after' (_, _, _, transitions, initialState) [] = [initialState]  
after' (states, labelIn, labelOut, transitions, initialState) (label:traceTail) = do
    let possibleNextStates = nextState initialState label transitions
    concatMap (\next -> after' (states, labelIn, labelOut, transitions, next) traceTail) possibleNextStates

-- main function that finds final states. 
-- It first filters out all delta's and then try to find final states
-- then having final states it addes all states that are reachable by tau transitions
after :: IOLTS -> Trace -> [State]
after (states, lsi, lso, transitions, initialState) (trace) = do 
    let result = after' (states, lsi, lso, transitions, initialState) (removeDeltas trace)
    addTauTransitions result transitions

coffeeMachine1 :: IOLTS
coffeeMachine1 = createIOLTS [(1, "?coin", 2), (2, "!tea", 3), (2, "!coffee", 4)]

coffeeMachine2 :: IOLTS
coffeeMachine2 = createIOLTS [(1, "?coin", 2), (1, "?coin", 3), (2, "!tea", 4), (3, "!coffee", 5)]

-- define nextTransitions' for IOLTS
iolts_nextTransitions' :: [LabeledTransition] -> State -> [(State, Label)]
iolts_nextTransitions' lt q0 = [(s', l) | (s, l, s') <- lt, s == q0]

-- Find following transitions for IOLTS
iolts_findFollowingTransitions' :: [LabeledTransition] -> [State] -> [Label] -> [([State], [Label])]
iolts_findFollowingTransitions' lt st ls = [(s' : st, ls ++ [l]) | (s', l) <- iolts_nextTransitions' lt (head st)]

-- Generate traces recursively for IOLTS
iolts_traces' :: [LabeledTransition] -> [([State], [Label])] -> [([State], [Label])]
iolts_traces' _ [] = []
iolts_traces' lt pairs = pairs ++ iolts_traces' lt next
  where next = concatMap (uncurry $ iolts_findFollowingTransitions' lt) pairs

-- traces function for IOLTS
iolts_traces :: IOLTS -> [Trace] -- [[Label]]
iolts_traces (_, _, _, lt, q0) = nub $ map snd (traces' lt [([q0], [])])    

nextTransitions :: State -> [Label] -> [LabeledTransition] -> [(State,Label)]
nextTransitions s0 lsi ts = do
    -- Normal Transitions
    let transitions = [(s', l) | (s,l,s')<- ts , s == s0]

    -- If a transition happens because of an input label, create an alternative delta transition
    -- If transitions are empty, then add a delta transition, because its a terminal node
    let dTransitions = if not (null transitions) then nub [(s0, delta) | (s,l) <- transitions, l `elem` lsi] else [(s0, delta)]

    dTransitions ++ transitions

findfollowingtransitions :: [Label] -> [LabeledTransition] -> [State] -> [Label] -> [([State],[Label])]
findfollowingtransitions lsi ts ss ls = filter (\(a,b) -> length b <= 5) [(s:ss,ls++[l])| (s,l)<- nextTransitions (head ss) lsi ts ]

straces':: [Label] -> [LabeledTransition] -> [([State],[Label])]-> [([State],[Label])]
straces' lsi ts [] = []
straces' lsi ts pairs = pairs ++ straces' lsi ts next
    where next = concatMap (uncurry $ findfollowingtransitions lsi ts) pairs

straces :: IOLTS -> [Trace]
straces (ss, lsi, lso, ts, s0) = nub $ map snd (straces' lsi ts [([s0],[])])


-- Function to generate tuples of (trace, after)
generateTraceAfterTuples :: IOLTS -> [(Trace, [State])]
generateTraceAfterTuples iolts = map (\trace -> (trace, after iolts trace)) (iolts_traces iolts)

-- Function to generate tuples of (trace, after)
generateStraceAfterTuples :: IOLTS -> [(Trace, [State])]
generateStraceAfterTuples iolts = map (\trace -> (trace, after iolts trace)) (straces iolts)

main :: IO ()
main = do
    print $ generateTraceAfterTuples coffeeMachine1 -- [([],[1]),(["coin"],[2]),(["coin","coffee"],[4]),(["coin","tea"],[3])]
    print $ generateTraceAfterTuples coffeeMachine2 -- [([],[1]),(["coin"],[2,3]),(["coin","tea"],[4]),(["coin","coffee"],[5])]
    print $ generateStraceAfterTuples coffeeMachine1 -- For example, because straces are infinite:
    -- [([],[1]),(["delta"],[1]),(["coin"],[2]),(["delta","delta"],[1]),(["delta","coin"],[2]),(["coin","coffee"],[4]),(["coin","tea"],[3]),(["delta","delta","delta"],[1]),(["delta","delta","coin"],[2]),(["delta","coin","coffee"],[4]),(["delta","coin","tea"],[3]),(["coin","coffee","delta"],[4]),(["coin","tea","delta"],[3]),(["delta","delta","delta","delta"],[1]),(["delta","delta","delta","coin"],[2]),(["delta","delta","coin","coffee"],[4]),(["delta","delta","coin","tea"],[3]),(["delta","coin","coffee","delta"],[4]),(["delta","coin","tea","delta"],[3]),(["coin","coffee","delta","delta"],[4]),(["coin","tea","delta","delta"],[3]),(["delta","delta","delta","delta","delta"],[1]),(["delta","delta","delta","delta","coin"],[2]),(["delta","delta","delta","coin","coffee"],[4]),(["delta","delta","delta","coin","tea"],[3]),(["delta","delta","coin","coffee","delta"],[4]),(["delta","delta","coin","tea","delta"],[3]),(["delta","coin","coffee","delta","delta"],[4]),(["delta","coin","tea","delta","delta"],[3]),(["coin","coffee","delta","delta","delta"],[4]),(["coin","tea","delta","delta","delta"],[3])]
    print $ generateStraceAfterTuples coffeeMachine2 -- For example, because straces are infinite:
    -- [([],[1]),(["delta"],[1]),(["coin"],[2,3]),(["delta","delta"],[1]),(["delta","coin"],[2,3]),(["coin","tea"],[4]),(["coin","coffee"],[5]),(["delta","delta","delta"],[1]),(["delta","delta","coin"],[2,3]),(["delta","coin","tea"],[4]),(["delta","coin","coffee"],[5]),(["coin","tea","delta"],[4]),(["coin","coffee","delta"],[5]),(["delta","delta","delta","delta"],[1]),(["delta","delta","delta","coin"],[2,3]),(["delta","delta","coin","tea"],[4]),(["delta","delta","coin","coffee"],[5]),(["delta","coin","tea","delta"],[4]),(["delta","coin","coffee","delta"],[5]),(["coin","tea","delta","delta"],[4]),(["coin","coffee","delta","delta"],[5]),(["delta","delta","delta","delta","delta"],[1]),(["delta","delta","delta","delta","coin"],[2,3]),(["delta","delta","delta","coin","tea"],[4]),(["delta","delta","delta","coin","coffee"],[5]),(["delta","delta","coin","tea","delta"],[4]),(["delta","delta","coin","coffee","delta"],[5]),(["delta","coin","tea","delta","delta"],[4]),(["delta","coin","coffee","delta","delta"],[5]),(["coin","tea","delta","delta","delta"],[4]),(["coin","coffee","delta","delta","delta"],[5])]

-- Test report:
-- I tested it against both traces and straces, when generating straces I had to set a limit on depth, because they are infinite.
-- Also if you have transitions from state 1 to state 1, there is a possibliity of an infinite loop, and those cases are not covered here.

-- Time Spent: 180 min
