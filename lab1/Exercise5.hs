-- Crime Scene Investigation
-- A group of five school children is caught in a crime. One of them has stolen something from 
-- some kid they all dislike. The headmistress has to find out who did it. She questions the children, 
-- and this is what they say:
-- Matthew: Carl didn't do it, and neither did I.
-- Peter: It was Matthew or it was Jack.
-- Jack: Matthew and Peter are both lying.
-- Arnold: Matthew or Peter is speaking the truth, but not both.
-- Carl: What Arnold says is not true.
-- Their class teacher now comes in. She says: three of these boys always tell the truth, and two 
-- always lie. You can assume that what the class teacher says is true. 
-- Use Haskell to write a function that computes who was the thief, and a function that 
-- computes which boys made honest declarations. Make sure you comment on how you 
-- Lab 1  Week 23. Pre/Post Conditions, Hoare Logic and Propositional Logic6converted natural languages into the specifications.
-- Here are some definitions to get you started. Describe the process you followed. Code without 
-- explanation will be deemed insufficient.
-- data Boy = Matthew | Peter | Jack | Arnold | Carl
--             deriving (Eq,Show)
-- boys = [Matthew, Peter, Jack, Arnold, Carl]
-- You should first define a function
-- accuses :: Boy -> Boy -> Bool
-- for encoding whether a boy accuses another boy.
-- Next, define
-- accusers :: Boy -> [Boy]
-- giving the list of accusers of each boy.
-- Finally, define
-- guilty, honest :: [Boy]
-- to give the list of guilty boys, plus the list of boys who made honest (true) statements.
-- If the puzzle is well-designed, then  guilty  should give a singleton list.
-- Deliverables: Haskell program, explanation, indication of time spent.


data Boy = Matthew | Peter | Jack | Arnold | Carl
            deriving (Eq,Show)
            
boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew x = (x /= Carl) && (x /= Matthew)
accuses Peter x = (x == Matthew) || (x == Jack)
accuses Jack x = not (accuses Matthew x) && not (accuses Peter x)
accuses Arnold x = (accuses Matthew x || accuses Peter x) && not (accuses Matthew x && accuses Peter x)
accuses Carl x = not (accuses Arnold x)

accusers :: Boy -> [Boy]
accusers boy = filter (`accuses` boy) boys


guilty :: Boy
guilty = head [boy | boy <- boys, length (accusers boy) == 3]

-- Determine the honest boys based on the guilty boy
honest :: [Boy]
honest = [boy | boy <- boys, accuses boy guilty]
main :: IO ()
main = do
    print guilty
    print honest
