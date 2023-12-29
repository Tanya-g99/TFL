module AnalysInvalidInfixes where

import LR0Parser
import Grammar
import EarlyAlgorithm
import LR0

calculateErrorPositions :: Int -> Int -> Int -> (Int, Int)
calculateErrorPositions i j length
    | j > (length - i + 1) = (length - j + 1, length - j + 1)
    | otherwise = (i, j)

parse :: String -> String -> String
parse strGrammar string = let 
    grammar = initGrammar strGrammar
    reversedGrammar = reverseGrammar grammar
    reversedString = reverse string

    lr0Parser = case initLR0Parser grammar of
        NotLR0 err -> error err
        _ -> initLR0Parser grammar
    errorPositions errIndex = calculateErrorPositions errIndex (earlyAlgorithm reversedGrammar reversedString) (length string)

    in case (parseString lr0Parser string) of
        Yes -> "The string " ++ string ++ " is accepted"
        Not errIndex m -> show (errorPositions errIndex)
