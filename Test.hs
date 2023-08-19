module Test where

testPassed computed expected
    | computed == expected = "Passed!" 
    | otherwise = "Failed (Computed: " ++ show computed ++ ", Expected: " ++ show expected ++ ")"

test label func inp expected = do
    putStrLn (label ++ " " ++ show inp ++ " == " ++ show expected)
    putStrLn (testPassed (func inp) expected)
    putStrLn ""
