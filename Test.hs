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

testSummary label inp expected =
    bright Yellow label ++ " " ++ coloredShow inp ++ colored Red " == " ++ coloredShow expected 

testOutput label inp expected computed = do
    putStrLn $ testSummary label inp expected 
    putStrLn $ testPassed computed expected
    putStrLn ""

showSp x = show x ++ " " 

test label func inp expected = do
    testOutput label inp expected (func inp) 
    
test2 label func (inp1, inp2) expected = do
    testOutput label 
        (showSp inp1 ++ show inp2) 
        expected 
        (func inp1 inp2) 

test3 label func (inp1, inp2, inp3) expected = do
    testOutput label 
        (showSp inp1 ++ showSp inp2 ++ show inp3) 
        expected 
        (func inp1 inp2 inp3) 

test4 label func (inp1, inp2, inp3, inp4) expected = do
    testOutput label 
        (showSp inp1 ++ showSp inp2 ++ showSp inp3 ++ show inp4) 
        expected 
        (func inp1 inp2 inp3 inp4) 

test5 label func (inp1, inp2, inp3, inp4, inp5) expected = do
    testOutput label 
        (showSp inp1 ++ showSp inp2 ++ showSp inp3 ++ showSp inp4 ++ show inp5) 
        expected 
        (func inp1 inp2 inp3 inp4 inp5) 
