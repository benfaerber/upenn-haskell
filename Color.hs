module Color where

data TextModifer = Bold | Underline | NoUnderline | Default
data ColorModifier = Normal | Bright 

data Color =
    Red | Green | Blue | White | Magenta | Cyan | Yellow

cc Red = "1" 
cc Green = "2"
cc Yellow = "3"
cc Blue = "4"
cc Magenta = "5"
cc Cyan = "6"
cc White = "7"
getColorCode = cc 

getColorModifier Bright = "9"
getColorModifier Normal = "3"

mm Bold = "1"
mm Underline = "4"
mm NoUnderline = "24"
mm Default = "0"
getTextModifierCode = mm

getTextCmd content = "\ESC[" ++ content ++ "m" 
getColorCmd modifier color = getTextCmd $ getColorModifier modifier ++ getColorCode color 
getModifierCmd modifier = getTextCmd $ getTextModifierCode modifier

makeColoredText modifier color text = getColorCmd modifier color ++ text ++ getModifierCmd Default 
makeModifiedText modifier text = getModifierCmd modifier ++ text ++ getModifierCmd Default

colored = makeColoredText Normal  
bright = makeColoredText Bright 
underline = makeModifiedText Underline 
bold = makeModifiedText Bold
