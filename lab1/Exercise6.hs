module Exercise6 where
import Lecture3

flat :: Form -> Form
flat f@(Prop x) = f
flat (Neg x) = Neg (flat x)
flat (Dsj xs) = Dsj
    (concatMap (\x -> case x of
        Dsj y -> map flat y
        _ -> [flat x]) xs)
flat (Cnj xs) = Cnj
    (concatMap (\x -> case x of
        Cnj y -> map flat y
        _ -> [flat x]) xs)

isDnf :: Form -> Bool
isDnf (Dsj xs) = all
    (\x -> case x of
        (Cnj ys) -> all (\y -> case x of
            (Prop a) -> True
            (Neg(Prop a)) -> True

            _ -> False) ys
        _ -> False) xs
isDnf _ = False

isCnf :: Form -> Bool
isCnf (Cnj xs) = all
    (\x -> case x of
        (Dsj ys) -> all (\y -> case x of
            (Prop a) -> True
            (Neg(Prop a)) -> True

            _ -> False) ys
        _ -> False) xs
isCnf _ = False

toCnf :: Form -> Form
toCnf f = flat $ nnf $ arrowfree f

toCnf0 :: Form -> Form
toCnf0 f =  nnf $ arrowfree f
cnf :: Form -> Form
cnf f
  | isCnf f = f
  | isDnf f = nnf (Neg f)
  | otherwise = toCnf f


