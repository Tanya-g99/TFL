module TestingFSM where

import Matrix
import FSM

makeAdjacencyMatrix :: Int -> [Transition] -> Matrix
makeAdjacencyMatrix statesSize transitions = let 
    coef i j = (lookupMy i j transitions) where
        lookupMy :: Int -> Int -> [Transition] -> Int
        lookupMy _ _ [] = 0
        lookupMy i j ((q1, q2, _):transitions) 
            | (i == q1 && j == q2) = 1
            | otherwise = lookupMy i j transitions 
    in ([[coef i j 
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

testTestingFSM1 :: [Transition]
testTestingFSM1 = let
    (_, _, _, transitions, _) = makeIntFSM "ab|a"
    in transitions

testTestingFSM :: Matrix
testTestingFSM = let
    (states, _, _, transitions, _) = makeIntFSM "ab|a"
    in makeReachabilityMatrix(makeAdjacencyMatrix (length states) transitions)
