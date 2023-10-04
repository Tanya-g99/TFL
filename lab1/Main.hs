module Main where
import Data.List (elemIndices)
type Matrix = [[String]]

makeEmptyMatrix :: Int -> Int -> Matrix
makeEmptyMatrix rows cols 
    | (rows > 0) && (cols > 0) = replicate rows (replicate cols "")
    | otherwise = error "Matrix cols and rows must be > 0"

expandMatrix :: String -> Int -> [[String]]
expandMatrix elem size = [[ elem ++ (show i) ++ (show j) | i <- [0 .. (size - 1)]]| j <- [0 .. 1]]

replaceAtIndex :: Int -> Int -> String -> Matrix -> Matrix  
replaceAtIndex i j x matrix = let replaceAtIndex' i x xs = take i xs ++ [x] ++ drop (i+1) xs 
                                in replaceAtIndex' i (replaceAtIndex' j x (matrix !! i)) matrix

matrixSize :: Matrix -> Int
matrixSize a = length $ a !! 0

articSum :: Matrix -> Matrix -> Matrix
articSum a b = replaceAtIndex 0 0 (articEl (a !! 0 !! 0) (b !! 0 !! 0))
                (replaceAtIndex 1 0 (articEl (a !! 1 !! 0) (b !! 1 !! 0)) (makeEmptyMatrix 2 1))
    where articEl a b = "(arcsum " ++ a ++ " " ++ b ++ ")"

articMult :: Matrix -> Matrix -> Matrix
articMult a b 
            | matrixSize b == 2 = let m = makeEmptyMatrix 2 2 
                                    in replaceAtIndex 0 0 (articEl (a !! 0 !! 0) (b !! 0 !! 0) (a !! 0 !! 1) (b !! 1 !! 0))
                                        (replaceAtIndex 0 1 (articEl (a !! 0 !! 0) (b !! 0 !! 1) (a !! 0 !! 1) (b !! 1 !! 1))
                                        (replaceAtIndex 1 0 (articEl (a !! 1 !! 0) (b !! 0 !! 0) (a !! 1 !! 1) (b !! 1 !! 0))
                                        (replaceAtIndex 1 1 (articEl (a !! 1 !! 0) (b !! 0 !! 1) (a !! 1 !! 1) (b !! 1 !! 1)) m)))
            | otherwise = let m = makeEmptyMatrix 2 1
                                    in replaceAtIndex 0 0 (articEl (a !! 0 !! 0) (b !! 0 !! 0) (a !! 0 !! 1) (b !! 1 !! 0))
                                        (replaceAtIndex 1 0 (articEl (a !! 1 !! 0) (b !! 0 !! 0) (a !! 1 !! 1) (b !! 1 !! 0)) m)
                                where articEl a b c d = "(arcmax (arcsum " ++ a ++ " " ++ b ++ ") (arcsum " ++ c ++ " " ++ d ++ "))"
      
articGg :: Matrix -> Matrix -> [String]
articGg a b = let articGg' ab = ["(arcgg " ++ (ab !! 0) ++ " " ++ (ab !! 1) ++ ")"]
                in (foldr (++) [] (map articGg' [ [a !! i !! j, b !! i !! j] | i <- [0 .. (length a - 1)], j <- [0 .. (length (a !! 0) - 1)] ]))

contains :: Eq a => [a] -> a -> Bool
contains arr elem = (or [elem == elem' | elem' <- arr])

delChars :: [Char] -> String -> [String]
delChars chars expr = case dropWhile (contains chars) expr of
                    "" -> []
                    expr' -> w : delChars chars expr''
                        where (w, expr'') = break (contains chars) expr'
                            
split :: String -> [[String]]
split input = map (delChars " ->") (delChars "\n" input)

getFuncs :: [[String]] -> String
getFuncs arr = let 
                    getFuncs' "" res = res
                    getFuncs' arr res = 
                        if elem (head arr) res 
                        then getFuncs' (tail arr) res
                        else getFuncs' (tail arr) $ (arr !! 0) : res
    in getFuncs' (foldr (++) "" [ f | i <- arr, f <- i]) ""

makeInequality1 :: [[String]] -> String -> String
makeInequality1 exprs funcs = let
    expandMatrix' el = (expandMatrix ("a" ++ (show (el !! 0))) 2)
    getMult expr funcs res 
        | (length expr) == 0 = res
        | otherwise = getMult (tail expr) funcs (articMult res (expandMatrix' (elemIndices (expr !! 0) funcs)))
    arcgg a b = 
        foldl (++) "" [ ("(arcgg " ++ (a !! i !! j) ++ " " ++ (b !! i !!j) ++ ")") 
            | i <- [0 .. ((length a) - 1)], j <- [0 .. ((length (a !! 0)) - 1)]]
    getMult' i j = getMult (tail(exprs !! i !! j)) funcs (expandMatrix' (elemIndices (exprs !! i !! j !! 0) funcs))

    in "(assert (and " ++ (foldl (++) "" [(arcgg (getMult' i 0) (getMult' i 1)) | i <- [0 .. (length exprs) - 1]]) ++ "))\n"

getFreeСoefs :: String -> String -> [[String]]
getFreeСoefs expr funcs = let 
    getFreeСoefs' i_fun s res expr funcs 
        | (i_fun /= ((length expr) - 1)) = (getFreeСoefs' (i_fun + 1) (new_s " a") ((new_s " b") : res) expr funcs) 
        | otherwise = map words ((new_s " b") : res)
        where new_s w = (s ++ w ++ (show ((elemIndices (expr !! i_fun) funcs) !! 0)))
    in getFreeСoefs' 0 "" [] expr funcs

makeInequality2 :: [[String]] -> String -> String 
makeInequality2 exprs funcs = let
    expandMatrix' coef = expandMatrix coef (if (coef !! 0) == 'a' then 2 else 1 )
    getMult2 arr = foldl (articMult) (expandMatrix' (arr !! 0)) (map (expandMatrix') (tail arr))
    getSum2 arr = (foldl (articSum) (arr !! 0) (tail arr))
    getInequality arr_coefs = getSum2 (map getMult2 arr_coefs)
    getFreeC2 funcs expr = foldr (++) "" ["(arcgg " ++ ((getInequality(getFreeСoefs (expr !! 0) funcs)) !! i !! j) ++ " " ++ ((getInequality(getFreeСoefs (expr !! 1) funcs))!! i !! j) ++ ")" 
        | i <- [0 .. ((length (getInequality(getFreeСoefs (expr !! 0) funcs))) - 1)], j <- [0 .. ((length ((getInequality(getFreeСoefs (expr !! 0) funcs)) !! 0)) - 1)]]

    in "(assert (and " ++ (foldl (++) "" (map (getFreeC2 funcs) exprs)) ++ "))\n"

getAllACoefs :: String -> [String]
getAllACoefs funcs = let getIndex i = map ((++)("a" ++ ii)) ["00", "01", "10", "11"] where ii = (show i)
    in foldl (++) [] ( map getIndex [i | i <- [0..((length funcs)-1)]])

getAllBCoefs :: String -> [String]
getAllBCoefs funcs = let getIndex i = map ((++)("b" ++ ii)) ["00", "01"] where ii = (show i)
    in foldl (++) [] ( map getIndex [i | i <- [0..((length funcs)-1)]])

makeInequalities :: [[String]] -> String 
makeInequalities exprs = smt2Format ((makeInequality1 exprs funcs) ++ (makeInequality2 exprs funcs)) funcs
    where funcs = getFuncs exprs

output :: String -> String
output body = 
    "(set-logic QF_NIA)\n\
    \(define-fun NEGATIVE_INFINITY () Int -1)\n\
    \(define-fun arcmax ((a Int) (b Int)) Int\n\
    \   (ite (>= a b) a b))\n\
    \(define-fun arcsum ((a Int) (b Int)) Int\n\
    \   (ite (and (> a NEGATIVE_INFINITY)  (> b NEGATIVE_INFINITY)) (+ a b) (ite (<= a NEGATIVE_INFINITY) b a)))\n\
    \(define-fun arcgg ((a Int) (b Int)) Bool\n\
    \   (or (> a b) (= a b NEGATIVE_INFINITY)))\n"
    ++ body ++ 
    "(check-sat)\n\
    \(get-model)\n\
    \(exit)\n"

smt2Format :: String -> String -> String
smt2Format inequalities funcs = let 
    all_coefs = (getAllACoefs funcs) ++ (getAllBCoefs funcs) 
    declare = (foldl (++) "" ["(declare-fun " ++ coef ++ " () Int)\n" | coef <- all_coefs])

    inequalitiesA = (foldl (++) "" 
        ["(assert (" ++ 
        (if (drop 2 coef) == "00" 
            then "> " 
            else ">= ") ++ 
        coef ++ 
        " -1))\n" | coef <- (getAllACoefs funcs)])

    inequalitiesB = (foldl (++) "" 
        ["(assert (" ++ 
        (if (drop 2 coef) == "00" 
            then "or (> " ++ coef ++ " -1) (and (= 0 " ++ (init coef) ++ "0) (= 0 " ++ coef ++" ))))\n" 
            else ">= " ++ coef ++ " -1))\n")
        | coef <- (getAllBCoefs funcs)])
    in output (declare ++ inequalitiesA ++ inequalitiesB ++ inequalities)

getLines = do 
    c <- getLine
    if c == "exit" then return ""
        else do 
            s <- getLines
            return (c ++ s)

main = do 
    input <- getLines
    let res = makeInequalities (split input)
    writeFile "file.smt2" res
