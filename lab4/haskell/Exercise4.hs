module Exercise4 where

import LTS
import Data.List
import Exercise2
import Exercise3

infixl 1 `after`

after' :: State -> Label -> [Label] -> [LabeledTransition] -> [State]
after' s l lsi ts = nub [next | (s',l',s'') <- ts, s == s' && (l == l' || (l == delta && l' `elem` lsi) ), let next = if l /= delta then s'' else s']
    
after :: IOLTS -> Trace -> [State]
(ss, lsi, lso, ts, s0) `after` [tr] = after' s0 tr lsi ts
(ss, lsi, lso, ts, s0) `after` (tr:trs) = do
    let next = after' s0 tr lsi ts

    nub $ concatMap (\s -> (ss,lsi,lso,ts,s) `after` trs ) next

