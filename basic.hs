module Test where
import Data.Tuple 

getSecondFrom :: a -> b -> c -> b
getSecondFrom a b c = b

--lamda
-- \x -> 2*x+7
-- \x -> \y -> ...
-- \x y -> ...


on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

-- class Eq1 a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> Bool

-- instance Eq1 Bool where
--     True == True = True
--     False == False = True
--     _ == _  = False
--     x /= y = not (x==y)

class Printable a where
    toString :: a -> [Char]

instance Printable Bool where
    toString True = "true"
    toString False = "false"

instance Printable () where
    toString () = "unit type"

instance (Printable a, Printable b) => Printable (a,b) where
    toString =  \a -> "(" ++ toString (fst a) ++ "," ++ toString (snd a) ++ ")"
