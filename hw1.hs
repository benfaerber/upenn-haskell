import Data.Char 

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

