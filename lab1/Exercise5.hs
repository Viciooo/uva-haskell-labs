data Boy = Matthew | Peter | Jack | Arnold | Carl
            deriving (Eq,Show)
            
boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
-- Modelling the relations from the task description
-- We can translate accusations of lying into accusations of guilt or innocence(here modeled as not guilt)
accuses Matthew x = (x /= Carl) && (x /= Matthew)
accuses Peter x = (x == Matthew) || (x == Jack)
accuses Jack x = not (accuses Matthew x) && not (accuses Peter x)
accuses Arnold x = (accuses Matthew x || accuses Peter x) && not (accuses Matthew x && accuses Peter x)
accuses Carl x = not (accuses Arnold x)

accusers :: Boy -> [Boy]
accusers boy = filter (`accuses` boy) boys


guilty :: Boy
-- A boy is guilty if he is accused by exactly 3 boys. 
-- We know that because the teacher said that exactly 3 of them tell the truth.
-- Had their statements been different, we would have had to consider more then one guilty or inconclusive results.
guilty = head [boy | boy <- boys, length (accusers boy) == 3]

honest :: [Boy]
-- We say that a boy is honest if he pointed out the guilty boy.
-- Since there were exactly 3 of them, which aligns with the teacher's statement,
-- we can be sure that those are the honest ones.
honest = [boy | boy <- boys, accuses boy guilty]
main :: IO ()
main = do
    print guilty
    print honest

-- Time spent: 60min
