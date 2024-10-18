module Exercise5 where

import Exercise4
import LTS
import Exercise3
import Data.List

-- List of bugs (found maunally)

    -- doorImpl1: Correct as stated in exercise description
    -- doorImpl2: when it closes, it returns opened instead of closed & when it opens, it returns closed instead of opened
    -- doorImpl3: when it unlocks, it transitions to the wrong state (2 instead of 1)
    -- doorImpl4: when it unlocks it returns state label (locked instead of unlocked), it is attributed to the wrong state & the same happens for locks transition (wrong stated label returned and wrong state attributed)
    -- doorImpl5: it return wrong stated label for open transition (open instead of opened)
    -- doorImpl6: the circle of opening and closing door leads to ann error (that doesnt happen in original implementation)
    -- doorImpl7: The trace ["close", "unlock", "open", "close", "lock", "unlock "] leads to error (this trace is not possible in the original impelemnation cause after close there cannot be transition unlock)
    -- doorImpl8: Transition 7 "close" 2 is impossible cause in state 7 doors are already closed

-- IOLTS forms for given door implementations (written manually)
doorModel :: IOLTS
doorModel = createIOLTS [
                (0,"?close", 1),(0,"!closed",1),
                (1,"?open", 0),(1,"!opened",0),
                (1, "?lock", 2), (1,"!locked",2),
                (2, "?unlock", 1), (2,"!unlocked",1)]

doorImpl2ToIOLTS :: IOLTS
doorImpl2ToIOLTS = createIOLTS [
                (0,"?close", 1),(0,"!opened",1),
                (1,"?open", 0),(1,"!closed",0),
                (1, "?lock", 2), (1,"!locked",2),
                (2, "?unlock", 1), (2,"!unlocked",1)]

doorImpl3ToIOLTS :: IOLTS
doorImpl3ToIOLTS = createIOLTS [
                (0,"?close", 1),(0,"!closed",1),
                (1,"?open", 0),(1,"!opened",0),
                (1, "?lock", 2), (1,"!locked",2),
                (2, "?unlock", 2), (2,"!unlocked",2)]

doorImpl4ToIOLTS :: IOLTS
doorImpl4ToIOLTS = createIOLTS [
                (0,"?close",1), (0, "!closed", 1),
                (1, "?open", 0), (1,"!opened",0),
                (1,"?unlock",2),(1,"!locked",2),
                (2,"?lock",1),(2,"!unlocked",1)]

doorImpl5ToIOLTS :: IOLTS
doorImpl5ToIOLTS = createIOLTS[
                    (0,"?close", 1),(0,"!closed",1),
                    (1,"?open", 0),(1,"!opened",0),
                    (1, "?lock", 2), (1,"!locked",2),
                    (2, "?unlock", 1), (2,"!unlocked",1)]

doorImpl6ToIOLTS :: IOLTS 
doorImpl6ToIOLTS = createIOLTS[
    (0, "?close", 1),(0,"!closed",1),
    (1, "?open", 3),(1,"!opened",3),
    (3, "?close", 4),(3,"!closed",4),
    (4, "?open", 5),(4,"!opened",5),
    (5, "?close", 6),(5,"!closed",6),
    (6, "?open", 6),(6,"delta",6),
    (1, "?lock", 2),(1,"!locked",2),
    (4, "?lock", 2),(1,"!locked",2),
    (6, "?lock", 2),(6,"!locked",2),
    (2, "?unlock", 1),(2,"!unlocked",1)]

doorImpl7ToIOLTS :: IOLTS
doorImpl7ToIOLTS = createIOLTS [
    (0,"?close", 1), (0,"!closed",1),
    (1,"?open", 0), (1,"!opened",0),
    (1, "?lock", 2), (1,"!locked",2),
    (2, "?unlock", 3), (2,"!unlocked",3),
    (4, "?close", 5), (4,"!closed",5),
    (3, "?open", 4), (3,"!opened",4),
    (3, "?lock", 2), (3,"!locked",2),
	(5, "?open", 0), (5,"!opened",0),
	(5, "?lock", 6), (5,"!locked",6)]

doorImpl8ToIOLTS :: IOLTS
doorImpl8ToIOLTS = createIOLTS [
    (0,"?close", 1),(0,"!closed",1),
    (1,"?open", 0),(1,"!opened",0),
    (1, "?lock", 2), (1,"!locked",2),
    (2, "?unlock", 3), (2,"!unlocked",3),
    (4, "?close", 5), (4,"!closed",5),
    (3, "?open", 4), (3,"!opened",4),
    (3, "?lock", 2), (3,"!locked",2),
    (5, "?open", 6), (5,"!opened",6),
    (5, "?lock", 2), (5,"!locked",2),
    (6, "?close", 7), (6,"!closed",7),
    (7, "?lock", 2), (7,"!locked",2),
    (7, "?close", 2), (7,"!closed",2)]

-- We can test corectness of others implemention but calucalating outputs of traces 
-- and checking if for given trace the implementaion output is subset of model output of the same trace 
-- in short we can check for ioco relation
-- model is the first implementation of the door 

-- First we need to implement out function (that calculates the output of the trace)
-- We will use the same function as in the previous exercise
out :: IOLTS -> [State] -> [Label]
out (_, _, labelOut, transitions, _) states = [label | (x, label, _) <- transitions,  x `elem` states, label `elem` labelOut]       

-- This function takes IOLTS , then caluclates after and based on that calculates out
calculateOut :: IOLTS -> Trace -> [Label]
calculateOut iolts trace = out iolts (after iolts trace)

-- This function removes tau from genrated straces (unfortunatelly our straces function generates traces with tau transitions)
removeTau :: Trace -> Trace
removeTau = filter (/= tau)

-- Thsi fuunction calucaltes all possible straces and their out for given IOLTS
-- then it removes delta and tau from traces, and removes duplicates
-- It returns a list of tuples (Trace, [Label])
calculateTracesWithOut :: IOLTS -> [(Trace, [Label])]
calculateTracesWithOut iolts = do
    let allTraces = straces iolts
    let fileterdTraces = nub (map (removeTau . removeDeltas) allTraces)
    let result = [(trace, calculateOut iolts trace) | trace <- fileterdTraces]
    result

isSubsetOf :: (Eq a) => [a] -> [a] -> Bool
isSubsetOf list1 list2 = null (list1 \\ list2)

findTupleByX :: (Eq a) => a -> [(a, b)] -> Maybe (a, b)
findTupleByX x = find (\(x', _) -> x' == x)

-- this function checks if all traces from model are present in the traces of implementation 
-- and  for given trace if out of implementation is subset of out model
checkSubsets :: (Eq a, Eq b) => [(a, [b])] -> [(a, [b])] -> Bool
checkSubsets modelList implList = all checkTuple modelList
  where
    checkTuple (x, y) = case findTupleByX x implList of
      Nothing -> False  
      Just (_, z) -> z `isSubsetOf` y 

-- This function checks if given implementation is ioco relation of model
iocoRelation :: IOLTS -> IOLTS -> Bool
iocoRelation model implem = do
    let modelOut = calculateTracesWithOut model
    let implemOut = calculateTracesWithOut implem
    checkSubsets modelOut implemOut

-- Unfortunatelly we weren't able to implement functions that creates IOLTS from door implementations
-- we tested it using manulaly created IOLTS and iocoRelation
-- tests returned False for implementations 2, 3, 4, 5, 6, and True for 7 and 8


-- Time spent: 180 min