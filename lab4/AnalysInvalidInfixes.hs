module AnalysInvalidInfixes where

import LR0Parser
import Grammar
import EarlyAlgorithm
import LR0

import Data.List (nub)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = nub

substringInRange :: Int -> Int -> String -> String
substringInRange start end str = take (end - start + 1) (drop start str) 


analysInfixes :: Grammar -> [String] -> (Int, Int) -> String -> [Int]
analysInfixes gr mm (k0, klast) word | (klast - k0) < 2 = removeDuplicates $ k0 : [klast]
                                     | otherwise = analysInfixes' gr klast mm [head (substringInRange (k0 + 1) (klast - 1) word)] (tail (substringInRange (k0 + 1) (klast - 1) word)) k0 [k0] (k0 + 1)

analysInfixes' :: Grammar -> Int ->  [String] -> String -> String -> Int ->  [Int] -> Int -> [Int]
analysInfixes' gr klast mm' subWord rest k0' errorsAr index      |   (klast <= (last errorsAr))  || (not (null newM)) && (length rest == 0) =   removeDuplicates $ errorsAr ++ [klast]
                                                                 |   (length rest == 0) &&  (null newM ) =  removeDuplicates $ (errorsAr ++ [indexError]) ++ [klast]
                                                                 |   null newM  = analysInfixes' gr klast [startGrammar] [head rest] (tail rest) indexError  (errorsAr ++ [indexError]) (index + 1)
                                                                 |   otherwise = analysInfixes' gr klast newM (subWord ++ [head rest]) (tail rest) k0' errorsAr (index + 1)
    where
   
        helper ::  [String] -> [String]  -> [String]
        helper [] newM' = newM'
        helper (start:ss) newM'        | earlyCheck start == 0 = helper ss newM' 
                                       | earlyCheck start == 1 = helper ss (start:newM')  
                                       | earlyCheck start == 2 = helper ss (start:newM')

        earlyCheck :: String -> Int
        earlyCheck start = earlyAlgorithm2 gr start subWord 
 
        newM = helper mm' []  

        startGrammar = getInitNterminal gr

        indexError :: Int
        indexError  = (head errorsAr) + index
 
              

calculateErrorPositions :: Int -> Int -> Int -> (Int, Int)
calculateErrorPositions i j length
    | i < (length - j - 1) = (length - j - 1, length - j - 1)
    | otherwise = (i, (length - j - 1))

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
        Not errIndex m -> let
            (k0, klast) = errorPositions errIndex
            in " Array of Errors: " ++ 
            (show  (if (k0 == klast) 
                then (analysInfixes grammar m (k0, klast) string)
                else reverse (analysInfixes grammar m (k0, klast) string)))

 


 
-- gg = parse gr wordd
-- gr = "S -> abS\nS -> c"
-- wordd = "adab"


 