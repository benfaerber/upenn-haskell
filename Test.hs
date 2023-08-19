module Test where

data TextModifer = Bold | Underline | NoUnderline | Default
data ColorModifier = Normal | Bright 

data Color =
    Red 
    | Green 
    | Blue
    | White
    | Magenta
    | Cyan
    | Yellow

getColorCode Red = "1" 
getColorCode Green = "2"
getColorCode Yellow = "3"
getColorCode Blue = "4"
getColorCode Magenta = "5"
getColorCode Cyan = "6"
getColorCode White = "7"

getColorModifier Bright = "9"
getColorModifier Normal = "3"

getTextModifierCode Bold = "1"
getTextModifierCode Underline = "4"
getTextModifierCode NoUnderline = "24"
getTextModifierCode Default = "0"

getColorCmd modifier color = "\ESC[" ++ getColorModifier modifier ++ getColorCode color ++ "m"
getModifierCmd modifier = "\ESC[" ++ getTextModifierCode modifier ++ "m"
makeColoredText modifier color text = getColorCmd modifier color ++ text ++ getColorCmd Normal White 
makeModifiedText modifier text = getModifierCmd modifier ++ text ++ getModifierCmd Default

colored = makeColoredText Normal  
bright = makeColoredText Bright 
underline = makeModifiedText Underline 
bold = makeModifiedText Bold

title name =
    underline (bright Blue name) 
    ++ "\n" 

testPassed computed expected
    | computed == expected = colored Green "Passed!" 
    | otherwise = 
        colored Red "Failed!" 
        ++ " (Computed: " ++ coloredShow computed ++ ")"

coloredType "True" = colored Blue "True"
coloredType "False" = colored Blue "False"
coloredType (x:xs) 
    | x == '[' = colored Cyan (x : xs)
    | x == '"' = colored Yellow (x : xs)
coloredType x = colored Magenta x

coloredShow x = coloredType (show x)

test label func inp expected = do
    putStrLn (bright Yellow label ++ " " ++ coloredShow inp ++ colored Red " == " ++ coloredShow expected) 
    putStrLn (testPassed (func inp) expected)
    putStrLn ""
