import Data.Char 
import Test

-- Not using Maybe for now, I need to learn more
charToDigit :: Char -> Integer 
charToDigit c
    | ord c >= 48 && ord c <= 57 = toInteger (ord c - 48)
    | otherwise = 0 

rev :: [Integer] -> [Integer]
rev = 
    let 
    rev' acc [] = acc
    rev' acc (x:xs) = rev' (x : acc) xs
    in rev' [] 

toDigits :: Integer -> [Integer]
toDigits x | x <= 0 = []
toDigits number =
    map charToDigit $ show number
    
toDigitsRev :: Integer -> [Integer]
toDigitsRev number = rev $ toDigits number

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst =
    let doubleEveryOther' acc [] = acc
        doubleEveryOther' acc (a:b:xs) = doubleEveryOther' (acc ++ [a, b*2]) xs
        doubleEveryOther' acc (a:xs) = acc ++ [a] 
    in rev $ doubleEveryOther' [] $ rev lst

sumDigits :: [Integer] -> Integer
sumDigits = foldr (\c acc -> sum (toDigits c) + acc) 0 

validate :: Integer -> Bool
validate number =
    let summed = sumDigits $ doubleEveryOther $ toDigits number in
    summed `mod` 10 == 0

main = do
    putStrLn (title "Homework 1")
    
    test "toDigits" toDigits 1234 [1,2,3,4]
    test "toDigitsRev" toDigitsRev 1234 [4,3,2,1]

    test "doubleEveryOther" doubleEveryOther [8,7,6,5] [16,7,12,5]
    test "doubleEveryOther" doubleEveryOther [1,2,3] [1,4,3]

    test "sumDigits" sumDigits [16,7,12,5] 22

    test "validate" validate 4012888888881881 True
    test "validate" validate 4012888888881882 False
    test "validate" validate 4929266301527013 True 
    test "validate" validate 4922246341527013 False 
