module Main where

derivative :: String -> Char -> String
derivative regularExpr symbol = 
    if (length regularExpr) > 20
        then "∅"
        else symbol : "-derivative " ++ regularExpr 

operates :: String
operates = "()|*#ϵ"

getAlphabet :: String -> String
getAlphabet regularExpr = let 
    getAlphabet' "" alphabet = alphabet
    getAlphabet' regularExpr alphabet =
        if not ((elem (head regularExpr) alphabet) || (elem (head regularExpr) operates))
            then getAlphabet' (tail regularExpr) $ (head regularExpr) : alphabet
            else getAlphabet' (tail regularExpr) alphabet 
    in getAlphabet' regularExpr ""

getStates :: String -> [(String, String)]
getStates regularExpr = let 
    getStates' "∅" alphabet way states = [] 
    getStates' regularExpr alphabet way states = 
        if elem regularExpr states
            then [(regularExpr, way)]
            else (regularExpr, way) : (foldl (++) [] [(getStates' (derivative regularExpr symbol) alphabet (symbol : way) (regularExpr : states)) | symbol <- alphabet])
    in getStates' regularExpr (getAlphabet regularExpr) "" []

main = do 
    regularExpr <- getLine
    let automation = getStates regularExpr
    putStrLn ""
