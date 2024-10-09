module Exercise7 where

import Exercise3
import Exercise5

-- Yes there is a difference. Assuming both receive a relation R where neither exist symmetric nor final transitive relationships.
-- Creating a symmetric closure on R will result in an additional relation per exisiting one. In transitive closure
-- additional relations will be created at a minimum of one per each two relations, assuming there is transitivity between elements.
-- If no relations in R have transitivity,i.e. no x,y,z in such that xRy and yRz, then the new relations will be 0.

-- By applying the symmetry before transitivity, we double the elements of R and with that create
-- new transitive connections between the existing relations that wasn't possible before

-- As we can see from the example, there is a difference between the two results.
counterExample = do
    putStr "symClos.trClos =" -- [(1,2),(1,3),(1,4),(2,1),(2,3),(2,4),(3,1),(3,2),(3,4),(4,1),(4,2),(4,3)]
    print $ symClos $ trClos [(1,2),(2,3),(3,4)]

    putStr "trClos.symClos =" -- [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)]
    print $ trClos $ symClos [(1,2),(2,3),(3,4)]
