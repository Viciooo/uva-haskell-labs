module Exercise3 where

import LTS
import Data.List
import Test.QuickCheck
import Exercise2


--1. Implement a function that returns all suspension traces of a given IOLTS

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


-- 2. Use your IOLTS generator and your straces function to create a random traces generator for QuickCheck
traceGen :: IOLTS -> Gen Trace
traceGen iolts = do
    n <- choose(10, 100) -- Choose a random amount of traces to generate

    let traces = take n $ straces iolts

    i <- choose(0, length traces - 1)

    return $ traces !! i

testTraceGen = do
    iolts <- generate ltsGen
    print iolts
    generate $ traceGen iolts


-- 3. Test your  straces  function using QuickCheck
-- ??


