-- Time Spent: 120 min 16:00

module Exercise5 where
import Data.Char (GeneralCategory(MathSymbol))

data Boy = Matthew | Peter | Jack | Arnold | Carl
            deriving (Eq,Show)

boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]


accuses :: Boy -> Boy -> Bool
{-Matthew: Carl didn't do it, and neither did I.-}
accuses Matthew y = (y /= Carl ) && (y /= Matthew)
{-Peter: It was Matthew or it was Jack.-}
accuses Peter y = (y == Matthew ) || (y == Jack)
{-Jack: Matthew and Peter are both lying.-}
accuses Jack y = not (accuses Matthew y) && not (accuses Peter y)
--Arnold: Matthew or Peter is speaking the truth, but not both
accuses Arnold y = (accuses Matthew y && not (accuses Peter y)) || (not (accuses Matthew y) && accuses Peter y)
--Carl: What Arnold says is not true.
accuses Carl y = not (accuses Arnold y)


accusers :: Boy -> [Boy]
accusers boy = filter (`accuses` boy) boys

guilty :: [Boy]
guilty = [boy | boy <- boys, length (accusers boy) == 3]

-- Determine the honest boys based on the guilty boy
honest :: [Boy]
honest = [boy | boy <- boys, accuses boy $ head guilty]

main :: IO ()
main = do
    print guilty
    print honest
