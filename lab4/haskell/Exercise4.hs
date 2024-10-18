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


a = ([0,1,2,3],["1","2","5"],["11","12","13","14"],[(0,"11",0),(0,"2",2),(1,"14",1),(1,"5",3),(2,"11",2),(2,"13",1),(2,"5",2),(3,"11",2),(3,"12",3),(3,"1",1),(3,"5",0)],0) `after` [delta, delta, "2"]