module Test where
import Data.Char

--functions    
main = putStrLn "Hello, world!"

sumSquares x y  = x * x + y * y

lenVec3 x y z = sqrt (x*x + y*y + z*z)

sign x = if x>0 then 1 else (if x<0 then (-1) else 0) 


--operators
infixl 6 |-|

a |-| b = if (a -b > 0) then a-b else 0-(a-b)


--basic types
discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standartDiscount :: Double -> Double
standartDiscount = discount 1000 5

test = isDigit '7'

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if (isDigit x && isDigit y) then (digitToInt x * 10 + digitToInt y ) else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt ((fst p2 - fst p1) ^ 2 + (snd p2 - snd p1)^2)

-- recursion

factorial n = if n == 0 then 1 else n * factorial (n - 1)

factorial' 0 = 1
factorial' n | n < 0  = error "arg must be > 0" 
             | n > 0  = n * factorial' (n - 1)

factorial'' :: Integer -> Integer
factorial'' n | n == 0 = 1
              | n < 0  = error "arg must be > 0" 
              | otherwise  = n * factorial'' (n - 1)

doubleFact :: Integer -> Integer
doubleFact n = if (n == 0 || n == 1) then 1 else n * doubleFact (n - 2)

fibonacci' 0 = 0
fibonacci' 1 = 1
fibonacci' n = fibonacci' (n - 1) + fibonacci' (n - 2)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci (-1) = 1
fibonacci n | n > 0 = helperPlus 1 0 n 2
            | n < 0 = helperMinus 1 0 n (-2)

helperPlus acc1 acc2 n i | n == i = acc1 + acc2 
                         | otherwise = helperPlus (acc1 + acc2) acc1 n (i + 1)

helperMinus acc1 acc2 n i | n == i = acc2 - acc1 
                          | otherwise = helperMinus (acc2 - acc1) acc1 n (i - 1)

seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3                          
seqA n = let
     helper k1 k2 k3 n i | n == i  = formula
                         | otherwise = helper formula k1 k2 n (i+1) 
        where formula = k1 + k2 - 2 * k3  
    in helper 3 2 1 n 3

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = let 
    helper sum count x | x < 10 = (sum + x, count + 1)
                       | otherwise = helper (x `mod` 10  + sum) (count + 1) (x `div` 10)
   in (helper 0 0 (abs(x)))
