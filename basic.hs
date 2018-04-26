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


class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool
    
class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool    

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a = 
        if (doesEnrageGork a && doesEnrageMork a) then stomp (stab a)
        else if (doesEnrageGork a) then stab a
        else if (doesEnrageMork a) then stomp a
        else a  

class (Eq a, Enum a, Bounded a) => SafeEnum a where
    ssucc :: a -> a
    ssucc a = 
        if (maxBound == a) then minBound
        else succ a

    spred :: a -> a
    spred a = 
        if (minBound == a) then maxBound
        else pred a

avg :: Int -> Int -> Int -> Double
avg a b c = let
    a' = toInteger a 
    b' = toInteger b
    c' = toInteger c 
    f = a' + b' + c'
    in fromInteger f / 3 
