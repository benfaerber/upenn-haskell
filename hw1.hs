import Data.Char 
import Control.Exception (assert)
import Test

-- Not using Maybe for now, I need to learn more
charToDigit :: Char -> Integer 
charToDigit c
    | ord c >= 48 || ord c <= 57 = toInteger (ord c - 48)
    | otherwise = 0 


revAux :: [Integer] -> [Integer] -> [Integer]
revAux acc [] = acc
revAux acc (x:xs) = revAux (x : acc) xs 

rev :: [Integer] -> [Integer]
rev = revAux [] 

toDigits :: Integer -> [Integer]
toDigits x | x <= 0 = []
toDigits number =
    map charToDigit (show number)
    
toDigitsRev :: Integer -> [Integer]
toDigitsRev number = rev (toDigits number)

auxDoubleEveryOther acc [] = acc
auxDoubleEveryOther acc (a:b:xs) = auxDoubleEveryOther (acc ++ [a, b*2]) xs
auxDoubleEveryOther acc (a:xs) = acc ++ [a] 

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst = rev (auxDoubleEveryOther [] (rev lst))

main = do
    putStrLn "Homework 1"
    
    test "toDigits" toDigits 1234 [1, 2, 3, 4]
    test "toDigitsRev" toDigitsRev 1234 [4, 3, 2, 1]

    test "doubleEveryOther" doubleEveryOther [8,7,6,5] [16,7,12,5]
    test "doubleEveryOther" doubleEveryOther [1,2,3] [1, 4, 3]
