-- Time Spent: 30 min
module Exercise3 where
import Test.QuickCheck.Property (Prop)


infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
   quicksort [ a | a <- xs, a < x ]
   ++ [x] ++ [a | a <- xs, a == x]
   ++ quicksort [ a | a <- xs, a > x ]

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker xs p q = stronger xs q p

data PropCustom = PropCustom {propName :: [Char], propFunc :: Int->Bool}

instance Show PropCustom where
    show = propName

instance Eq PropCustom where
    (==) a b = stronger [(-10)..10] (propFunc a) (propFunc b) && stronger [(-10)..10] (propFunc a) (propFunc b)

instance Ord PropCustom where
    compare a b
        | stronger [(-10)..10] (propFunc a) (propFunc b) && stronger [(-10)..10] (propFunc b) (propFunc a) = EQ
        | stronger [(-10)..10] (propFunc a) (propFunc b) && not (stronger [(-10)..10] (propFunc b) (propFunc a)) = GT
        | weaker [(-10)..10] (propFunc a) (propFunc b) && not (weaker [(-10)..10] (propFunc b) (propFunc a)) = LT

{-
Properties
-}
property1,property2,property3,property4  :: Int -> Bool
-- [4, 6, 8, 10]
property1 n = even n && n > 3

-- [-10,-8,-6,-4, 2, 0, 2, 4, 5, 6, 7, 8, 9, 10]
property2 n = even n || n > 3

-- [-10,-8,-6,-4, 2, 0, 2, 4, 6, 8, 10]
property3 n = (even n && n > 3) || even n

-- [-10,-8,-6,-4, 2, 0, 2, 4, 6, 8, 10]
property4 n = even n

propertiesList :: [PropCustom]
propertiesList = [PropCustom "Property1" property1, PropCustom "Property2" property2, PropCustom "Property3" property3 , PropCustom "Property4" property4]

-- Descending Order: property1/3/4, property2
strengthOrd :: IO()
strengthOrd = do
    let l = quicksort propertiesList
    print (reverse l)