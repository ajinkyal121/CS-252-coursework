{-
  Name: Ajinkya Lakade
  Class: CS 252
  Assigment: HW1
  Date: Feb 19 2022
  Description: Performs basic operation like addition, substraction, multiplication and power of for big numbers
-}

module BigNum (
  BigNum,
  bigAdd,
  bigSubtract,
  bigMultiply,
  bigEq,
  bigDec,
  bigPowerOf,
  prettyPrint,
  stringToBigNum,
) where

type Block = Int -- An Int from 0-999

type BigNum = [Block]

maxblock = 1000

bigAdd :: BigNum -> BigNum -> BigNum
bigAdd x y = bigAdd' x y 0
bigAdd' :: BigNum -> BigNum -> Block -> BigNum
bigAdd' [] [] 0 = []
bigAdd' [] [] z = [z]
bigAdd' x [] z = bigAdd' x [0] z
bigAdd' [] y z = bigAdd' [0] y z
bigAdd' (x:xs) (y:ys) z = r: bigAdd' xs ys q where (q, r) = divMod (x+y+z) maxblock


bigSubtract :: BigNum -> BigNum -> BigNum
bigSubtract x y =
  if length x < length y
    then error "Negative numbers not supported"
    else reverse $ stripLeadingZeroes $ reverse result
      where result = bigSubtract' x y 0

stripLeadingZeroes :: BigNum -> BigNum
stripLeadingZeroes (0:[]) = [0]
stripLeadingZeroes (0:xs) = stripLeadingZeroes xs
stripLeadingZeroes xs = xs

-- Negative numbers are not supported, so you may throw an error in these cases
bigSubtract' :: BigNum -> BigNum -> Block -> BigNum
bigSubtract' [] [] 0 = []
bigSubtract' [] [] _ = error "Negetive numbers not supported"
bigSubtract' x [] z = bigSubtract' x [0] z
bigSubtract' [] y z = error "Negetive numbers not supported"
bigSubtract' (x:xs) (y:ys) z
  | (x - z) < y       = x+1000-y-z : bigSubtract' xs ys 1
  | otherwise         = x-y-z : bigSubtract' xs ys 0

bigEq :: BigNum -> BigNum -> Bool
bigEq x y = x == y 

bigDec :: BigNum -> BigNum
bigDec x = bigSubtract x [1]

-- Handle multiplication following the same approach you learned in grade
-- school, except dealing with blocks of 3 digits rather than single digits.
-- If you are having trouble finding a solution, write a helper method that
-- multiplies a BigNum by an Int.
bigMultiply :: BigNum -> BigNum -> BigNum
bigMultiply [] [] = []
bigMultiply _ [] = []
bigMultiply [] _  = []
bigMultiply xs [y] = stripLeadingZeroes $ multiplyBlock xs y 0
bigMultiply xs (y:ys) = stripLeadingZeroes $ bigAdd (multiplyBlock xs y 0) (0:bigMultiply xs ys)

multiplyBlock :: BigNum -> Block -> Block -> BigNum
multiplyBlock [] y 0 = []
multiplyBlock [] y z = [z]
multiplyBlock _ 0 _ = [0]
multiplyBlock (x:xs) y z = r:multiplyBlock xs y q where (q,r) = divMod ((x*y)+z) maxblock


bigPowerOf :: BigNum -> BigNum -> BigNum
bigPowerOf _ [0] = [1]
bigPowerOf [] _ = [0]
bigPowerOf xs z = powerOf xs [1] z
    where powerOf x acc y
            | y > [0] = powerOf x (bigMultiply acc x) (bigDec y)
            | otherwise = acc

prettyPrint :: BigNum -> String
prettyPrint [] = ""
prettyPrint xs = show first ++ prettyPrint' rest
  where (first:rest) = reverse xs

prettyPrint' :: BigNum -> String
prettyPrint' [] = ""
prettyPrint' (x:xs) = prettyPrintBlock x ++ prettyPrint' xs

prettyPrintBlock :: Int -> String
prettyPrintBlock x | x < 10     = ",00" ++ show x
                   | x < 100    = ",0" ++ show x
                   | otherwise  = "," ++ show x

stringToBigNum :: String -> BigNum
stringToBigNum "0" = [0]
stringToBigNum s = stringToBigNum' $ reverse s

stringToBigNum' :: String -> BigNum
stringToBigNum' [] = []
stringToBigNum' s | length s <= 3 = read (reverse s) : []
stringToBigNum' (a:b:c:rest) = block : stringToBigNum' rest
  where block = read $ c:b:a:[]

sig = "9102llaf"