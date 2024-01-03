module AnalysInvalidInfixes where

import LR0Parser
import Grammar
import EarlyAlgorithm
import LR0

substringInRange :: Int -> Int -> String -> String
substringInRange start end str = take (end - start + 1) (drop start str) 

analysInfixes :: Grammar -> [String] -> (Int, Int) -> String -> [Int]
analysInfixes gr mm (k0, klast) word = analysInfixes' mm [head (substringInRange k0 klast word)] (tail (substringInRange k0 klast word)) k0 [k0]
     where 
        analysInfixes' :: [String] -> String -> String -> Int ->  [Int] -> [Int]
        analysInfixes' mm' subWord (r:rest) k0' errorsAr    | (null (newM mm' subWord)) && ( klast <= (last errorsAr))  || (not (null (newM mm' subWord))) && (null rest) = tail errorsAr
                                                            | (null rest) &&  (null (newM mm' subWord)) = tail (errorsAr ++ [indexError errorsAr subWord])
                                                            |  null (newM mm' subWord) = analysInfixes' [startGrammar] [r] rest (indexError errorsAr subWord) (errorsAr ++ [indexError errorsAr subWord])
                                                            |  otherwise = analysInfixes' (newM mm' subWord) (subWord ++ [r]) rest k0' errorsAr
   
        helper ::  [String] -> [String] -> String -> [String]
        helper [] newM _ = newM
        helper (start:ss) newM  str    | earlyCheck start str == 0 = helper ss newM str
                                       | earlyCheck start str == 1 = helper ss (start:newM) str
                                       | earlyCheck start str == 2 = helper ss (start:newM) str

        earlyCheck :: String -> String -> Int
        earlyCheck start subWord = earlyAlgorithm2 gr start subWord 
 
        newM :: [String] -> String -> [String]
        newM m str = helper m [] str

        startGrammar = getInitNterminal gr

        indexError :: [Int] -> String -> Int
        indexError errorsAr subWord = (head errorsAr) + (length subWord)
 
              

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




