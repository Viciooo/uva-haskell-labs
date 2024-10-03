-- Time Spent: 20 min
module Exercise5 where

import Data.List
import Exercise2

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos rel = do
    let newConn = rel @@ rel
    let newRel = nub $ rel ++ newConn

    if length newRel == length rel then sort newRel else trClos newRel
