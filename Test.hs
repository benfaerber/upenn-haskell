module Test where
import Color

title name =
    underline (bright Blue name) 
    ++ "\n" 

testPassed computed expected
    | computed == expected = colored Green "Passed!" 
    | otherwise = 
        colored Red "Failed!" 
        ++ " (Computed: " ++ coloredShow computed ++ ")"

coloredType t
    | t == "True" || t == "False" = colored Blue t
coloredType (x:xs) 
    | x == '[' = colored Cyan (x : xs)
    | x == '"' = colored Yellow (x : xs)
coloredType x = colored Magenta x

coloredShow x = coloredType $ show x

test label func inp expected = do
    putStrLn $ bright Yellow label ++ " " ++ coloredShow inp ++ colored Red " == " ++ coloredShow expected 
    putStrLn $ testPassed (func inp) expected
    putStrLn ""
     
