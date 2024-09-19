-- Time Spent: 120 min 16:00

module Exercise5 where

data Boy = Matthew | Peter | Jack | Arnold | Carl
            deriving (Eq,Show)

boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]


accuses :: Boy -> Boy -> Bool
accuses a b 
    {-Matthew: Carl didn't do it, and neither did I.
    M() = M && C = False <=> !(M && C) = True <=>
    <=> !M || !C = True
    -}
    -- | a == Matthew && b == Matthew = False
    -- | a == Matthew && b == Carl = False

    -- | a == Matthew && b == Peter = True
    -- | a == Matthew && b == Jack = True
    -- | a == Matthew && b == Arnold = True

    {-Peter: It was Matthew or it was Jack.
    P() = M || J = True
    -}
    -- | a == Peter && b == Matthew = True
    -- | a == Peter && b == Jack = True

    -- | a == Peter && b == Peter = False
    -- | a == Peter && b == Carl = False
    -- | a == Peter && b == Arnold = False

    {-Jack: Matthew and Peter are both lying.
    J() = !M() && !P() = True
    -}
    -- | a == Jack && b == Matthew = True
    -- | a == Jack && b == Carl = True
    -- | a == Jack && b == Peter = True

    -- | a == Jack && b == Arnold = False
    -- | a == Jack && b == Jack = False

    {-Arnold: Matthew or Peter is speaking the truth, but not both
    A() = M() xor P() = True <=>
    <=> (M() && !P()) || (!M() && P()) = True <=>
    <=> (M && C && !M && !J) || ((!M || !C) && (M || J)) = True <=>
    <=> False || ((!M || !C) && (M || J)) = True <=>
    <=> ((!M || !C) && (M || J)) = True 
    -}
    -- | a == Arnold && b == Matthew = True
    -- | a == Arnold && b == Carl = True
    -- | a == Arnold && b == Peter = True
    
    -- | a == Arnold && b == Arnold = False
    -- | a == Arnold && b == Jack = False
    
    {-Carl: What Arnold says is not true.
    C() = !A() = True <=>
    <=> !(!M || !C) || !(M || J) = True <=>
    <=> (M && C) || (!M && !J) = True
    -}
    -- | a == Carl && b == Matthew = True
    -- | a == Carl && b == Carl = True
    -- | a == Carl && b == Peter = True
    
    -- | a == Carl && b == Arnold = False
    -- | a == Carl && b == Jack = False

accusers :: Boy -> [Boy]
accusers b = [ x | x <- boys, accuses x b]
    