module GenerateString where

import Matrix
import FSM
import FSMtoReg
import Normalization

import System.Random
import Data.List

data Path = Path {
    state :: Int,
    string :: String
}

makeAdjacencyMatrix :: [[RegExp]] -> Matrix
makeAdjacencyMatrix lts = let
    regExprToInt r = 
        if r == Zero
            then 0
            else 1
    size = length lts
    in (map (map regExprToInt) lts,  size, size)

makeReachabilityMatrix :: Matrix -> Matrix
makeReachabilityMatrix (adjacencyMatrix, adjacencySize, _) = let
    identity = makeIdentityMatrix adjacencySize adjacencySize
    makeReachabilityMatrix' adjacency = let
        getReachabilityMatrix adjacency adjacencyN 1 res = (addMatrix res adjacencyN)
        getReachabilityMatrix adjacency adjacencyN n res =
            getReachabilityMatrix adjacency 
                                (mulMatrix adjacencyN adjacency)
                                (n-1)
                                (addMatrix res adjacencyN)
        in getReachabilityMatrix adjacency identity adjacencySize (makeEmptyMatrix adjacencySize adjacencySize)
    in makeReachabilityMatrix' (adjacencyMatrix, adjacencySize, adjacencySize)

getNextStates :: [Int] -> [Int]
getNextStates adjacency = let
    getNextStates' [] index res = res
    getNextStates' (a:adjacency) index res
        | a > 0 = getNextStates' adjacency (index + 1) (index:res)
        | otherwise = getNextStates' adjacency (index + 1) res
    in getNextStates' adjacency 0 []

generateIndex :: RandomGen g => g -> Int -> (Int, g)
generateIndex gen max = randomR (0, max - 1) gen

generateStateSeq :: RandomGen g => g -> [[Int]] -> [Int] -> [Int]
generateStateSeq gen adjacency finals = let
    generateStateSeq' gen adjacency (prevState:res) = let
        transitions = getNextStates (adjacency !! prevState)
        (index, g) = generateIndex gen (length transitions)
        transition = (transitions !! index)
        in if prevState `notElem` finals
            then generateStateSeq' g adjacency (transition:(prevState:res))
            else (reverse (prevState:res))
    in generateStateSeq' gen adjacency [0]
 
generateChar :: RandomGen g => g -> RegExp -> String
generateChar g regExp = 
    case regExp of
        Union chars -> let 
            (i, _) = generateIndex g (length chars)
            in regExpToString (chars !! i)
        _ -> regExpToString regExp

generateString :: RandomGen g => g -> [[RegExp]] -> [Int] -> String
generateString gen lts finals = let
    (adjacency, statesCount, _) = makeAdjacencyMatrix lts
    stateSeq = generateStateSeq gen adjacency finals
    getNextPositions visited (Path state string) = 
        [(Path nextState (string ++ (generateChar gen (lts !! state !! nextState))))
        | nextState <- [0..(statesCount-1)], 
        (nextState `notElem` visited)]
    bfs :: Int -> Int -> String
    bfs start end = let
        bfs' end visited [] = ""
        bfs' end visited ((Path state string):queue)
            | state == end = string
            | state `elem` visited = bfs' end visited queue
            | otherwise = bfs' end (state:visited) (queue++(getNextPositions visited (Path state string)))
        in bfs' end [] [Path start ""] 
    makeString :: [Int] -> String
    makeString stateSeq = let
        makeString' [] res = res
        makeString' [q1] res = res
        makeString' (q1:q2:[]) res = res ++ (bfs q1 q2)
        makeString' (q1:q2:stateSeq) res = makeString'(q2:stateSeq) (res ++ (bfs q1 q2))
        in makeString' stateSeq ""
    in makeString stateSeq

generateSortedArray :: RandomGen g => g -> Int -> Int -> (g, [Int])
generateSortedArray gen count max =
    (snd(randomR (0, count) gen),sort $ take count $ randomRs (0, (max - 1)) gen)

replace :: Int -> a -> [a] -> [a]
replace i x lst = take i lst ++ [x] ++ drop (i+1) lst

swap :: Int -> Int -> [a] -> [a]
swap i j lst = replace i (lst !! j) $ replace j (lst !! i) $ lst

generateSwapChar :: RandomGen g => (g, String) -> (g, String)
generateSwapChar (gen, string) = let
    (g, [firstCharIndex, secondCharIndex]) = generateSortedArray gen 2 (length string)
    in (g, (swap firstCharIndex secondCharIndex string))

replaceFragment :: Int -> Int -> [a] -> [a] -> [a]
replaceFragment i j x lst = take i lst ++ x ++ drop (j+1) lst

getFragment :: Int -> Int -> [a] -> [a]
getFragment i j lst = (take (j-i+1) (drop i lst))

swapFragments :: (Int, Int) -> (Int, Int) -> [a] -> [a]
swapFragments (i1, j1) (i2, j2) lst = 
    take (i1) lst
    ++ getFragment i2 j2 lst
    ++ getFragment (j1+1) (i2-1) lst
    ++ getFragment i1 j1 lst
    ++ drop (j2+1) lst

generateSwapFragments :: RandomGen g => (g, String) -> (g, String)
generateSwapFragments (gen, string) = let
    (g, [firstFragmentStart, firstFragmentFinish, secondFragmentStart, secondFragmentFinish]) = generateSortedArray gen 4 (length string)

    in (g, swapFragments (firstFragmentStart, firstFragmentFinish) (secondFragmentStart, secondFragmentFinish) string)

repeatChar :: Int -> String -> String
repeatChar index string = let
    repeatChar' indexChar "" i res = reverse res 
    repeatChar' indexChar (elem:string) i res
        | i == indexChar = repeatChar' indexChar string (i+1) (elem:(elem:res))
        | otherwise = repeatChar' indexChar string (i+1) (elem:res)
    in repeatChar' index string 0 ""

generateRepeatChar :: RandomGen g => (g, String) -> (g, String)
generateRepeatChar (gen, string) = let
    (charIndex, g) = generateIndex gen (length string)
    in (g, repeatChar charIndex string)

repeatFragment :: (Int, Int)-> String -> String
repeatFragment (start, end) string = 
    take (start) string
    ++ getFragment start end string
    ++ getFragment start end string
    ++ drop (end+1) string

generateRepeatFragment :: RandomGen g => (g, String) -> (g, String)
generateRepeatFragment (gen, string) = let
    (g, [fragmentStart, fragmentEnd]) = generateSortedArray gen 2 (length string)
    in (g, repeatFragment (fragmentStart, fragmentEnd) string)

generateRemoveChar :: RandomGen g => (g, String) -> (g, String)
generateRemoveChar (gen, string) = let
    (charIndex, g) = generateIndex gen (length string)
    in (g, (take charIndex string) ++ (drop (charIndex + 1) string))

generateRemoveFragment :: RandomGen g => (g, String) -> (g, String)
generateRemoveFragment (gen, string) = let
    (g, [fragmentStart, fragmentEnd]) = generateSortedArray gen 2 (length string)
    in (g, (take fragmentStart string) ++ (drop (fragmentEnd + 1) string))

-- При увеличении числа мутаций повышается вероятность неизменности строки
mutationsNumber :: Int
mutationsNumber = 7

mutateString :: RandomGen g => (g, String) -> String
mutateString (gen, string) = let
    (mutationIndex, g) = generateIndex gen mutationsNumber
    in if string == ""
        then "ϵ"
        else case mutationIndex of
            0 -> mutateString (generateSwapChar (g, string))
            1 -> mutateString (generateSwapFragments (g, string))
            2 -> mutateString (generateRepeatChar (g, string))
            3 -> mutateString (generateRepeatFragment (g, string))
            4 -> mutateString (generateRemoveChar (g, string))
            5 -> mutateString (generateRemoveFragment (g, string))
            _ -> string

checkWordMatchRegex :: String -> String -> Bool
checkWordMatchRegex regex string = let
    match' regex "" = nullable regex
    match' regex (char:string) = let
        deriv =  (getNormDerivative char regex)
        in if deriv == Zero 
            then False
            else match' deriv string
    in match' (regexToRegExp regex) string
