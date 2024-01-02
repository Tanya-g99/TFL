module AnalysInvalidInfixes where

import LR0Parser
import Grammar
import EarlyAlgorithm
import LR0
 

-- грамматика М (подслово - первоначально - первый символ после первой ошибки) (остаток слова в интервале ошибки *без первого символа) k0 klast (список ошибок - изначально с k0)
analysInfixes :: Grammar ->  [String] -> String -> String -> Int -> Int -> [Int] -> [Int]
analysInfixes gr mm subWord (r:rest) k0 klast errorsAr | checkError && checkEnd  || (not checkError) && checkEndOfWord = errorsAr
                                                       | checkEndOfWord && checkError = (errorsAr ++ [k0'])
                                                       | checkError = analysInfixes gr [startGrammar] (subWord ++ [r]) rest k0' klast (errorsAr ++ [k0'])
                                                       | otherwise = analysInfixes gr newM' (subWord ++ [r]) rest k0 klast errorsAr
    where 
        helper ::  [String] -> [String] -> [String]
        helper (start:ss) newM         | earlyCheck start == 0 = helper ss newM
                                       | earlyCheck start == 1 = helper ss (start:newM)
                                       | earlyCheck start == 2 = helper ss (start:newM)
        earlyCheck :: String -> Int
        earlyCheck start = earlyAlgorithm2 gr start subWord 

        newM' = helper mm []

        checkError :: Bool
        checkError = null newM'

        startGrammar = getInitNterminal gr
        checkEnd :: Bool
        checkEnd = ( klast <= (last errorsAr))

        checkEndOfWord = null rest
        k0' = (head errorsAr) + (length subWord)

                     

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




