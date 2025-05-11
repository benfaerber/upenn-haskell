import Test

type Peg = String
type Move = (Peg, Peg)

type Disks = [Integer]
-- a c
-- a b
-- c b

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 p1 p2 p3 = [(p1, p3)]
hanoi n p1 p2 p3 = 
    hanoi (n - 1) p1 p3 p2
    ++ [(p1, p3)]
    ++ hanoi (n-1) p2 p1 p3

main = do
    putStrLn $ title "Homework 1 - Tower of Hanoi"
    print (hanoi 15 "a" "b" "c") 
