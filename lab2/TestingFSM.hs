module TestingFSM where

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

makeAdjacencyMatrix :: Int -> [Transition] -> Matrix
makeAdjacencyMatrix statesSize transitions = let 
    coef i j = (lookupMy i j transitions) where
        lookupMy :: Int -> Int -> [Transition] -> Int
        lookupMy _ _ [] = 0
        lookupMy i j ((q1, q2, _):transitions) 
            | (i == q1 && j == q2) = 1
            | otherwise = lookupMy i j transitions 
    in ([[coef j i 
        | i <- [0..(statesSize-1)]] 
        | j <- [0..(statesSize-1)]]
    , statesSize, statesSize)

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
generateIndex gen max = randomR (0, max) gen

generateStateSeq :: RandomGen g => g -> [[Int]] -> [Int] -> [Int]
generateStateSeq gen adjacency finals = let
    generateStateSeq' gen adjacency (prevState:res) = let
        transitions = getNextStates (adjacency !! prevState)
        (index, g) = generateIndex gen ((length transitions) - 1)
        transition = (transitions !! index)
        in if transition `notElem` finals
            then generateStateSeq' g adjacency (transition:(prevState:res))
            else (reverse (transition:(prevState:res)))
    in generateStateSeq' gen adjacency [0]
 
generateChar :: RandomGen g => g -> RegExp -> String
generateChar g regExp = 
    case regExp of
        Union chars -> let 
            (i, _) = generateIndex g ((length chars)-1)
            in regExpToString (chars !! i)
        _ -> regExpToString regExp

generateString :: RandomGen g => g -> Matrix -> [[RegExp]] -> [Int] -> String
generateString gen (adjacency, statesCount, _) lts finals = let
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

generate10Strings :: String -> [String]
generate10Strings regex = let
    (states, start, finals, transitions, alphabet) = makeIntFSM regex
    (lts, _) = toLTS (states, start, finals, transitions, alphabet)
    in [generateString (mkStdGen i) (makeAdjacencyMatrix (length states) transitions) lts finals |
        i <- [0..10]]
