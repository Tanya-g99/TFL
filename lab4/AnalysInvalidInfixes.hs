module AnalysInvalidInfixes where

import LR0Parser
import Grammar
import EarlyAlgorithm
import LR0

import Data.List (nub)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = nub

--Получаем подслово которое внутри интервала 2х ошибок (   )
substringInRange :: Int -> Int -> String -> String
substringInRange start end str = take (end - start + 1) (drop start str) 

--проверяем, что длина слова будет не нуль
analysInfixes :: Grammar -> [String] -> (Int, Int) -> String -> [Int]
analysInfixes gr mm (k0, klast) word | (klast - k0) < 2 = removeDuplicates $ k0 : [klast]
                                     | otherwise = analysInfixes' gr klast mm [head (substringInRange (k0 + 1) (klast - 1) word)] (tail (substringInRange (k0 + 1) (klast - 1) word)) k0 [k0] (k0 + 1)

--Грамматика (последняя ошибка) М (разбираемое подслово) (остаток подслов) (массив ошибок) (текущий индекс в слове)
analysInfixes' :: Grammar -> Int ->  [String] -> String -> String -> Int ->  [Int] -> Int -> [Int]
analysInfixes' gr klast mm' subWord rest k0' errorsAr index      |   (klast <= (last errorsAr))  || (not (null newM)) && (length rest == 0) =   removeDuplicates $ errorsAr ++ [klast]
                                                                 |   (length rest == 0) &&  (null newM ) =  removeDuplicates $ (errorsAr ++ [indexError]) ++ [klast]
                                                                 |   null newM  = analysInfixes' gr klast [startGrammar] [head rest] (tail rest) indexError  (errorsAr ++ [indexError]) (index + 1)
                                                                 |   otherwise = analysInfixes' gr klast newM (subWord ++ [head rest]) (tail rest) k0' errorsAr (index + 1)
    where
   
        helper ::  [String] -> [String]  -> [String]
        helper [] newM' = newM'
        helper (start:ss) newM'        | earlyCheck start == 0 = helper ss newM'  -- не является элементом языка
                                       | earlyCheck start == 1 = helper ss (start:newM')   -- является суффиксом 
                                       | earlyCheck start == 2 = helper ss (start:newM')  -- является точным инфиксом

        earlyCheck :: String -> Int
        earlyCheck start = earlyAlgorithm2 gr start subWord 
 --В newM мы ищем Нетерминалы, которые при становлении стартовыми - принимает подслово (то есть это подслово является инфиксом или суффиксом) 
 -- Очев, если после helper   newM - пусто, следовательно индекс слова - ошибка
        newM = helper mm' []  
--если  newM - пусто, следовательно новое М - содержит только стартовый элемент
        startGrammar = getInitNterminal gr
--в indexError в (head errorsAr) хранится индекс первой ошибки, чтобы найти индекс текущей прибавляем .........
        indexError :: Int
        indexError  = (head errorsAr) + index
 
              

calculateErrorPositions :: Int -> Int -> Int -> (Int, Int)
calculateErrorPositions i j length
    | i > (length - j - 1) = (length - j - 1, length - j - 1)
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
            in "Array of Errors: " ++ 
            (show  (if (k0 == klast) 
                then reverse (analysInfixes grammar m (k0, klast) string)
                else (analysInfixes grammar m (k0, klast) string)))

 


 
-- gg = parse gr wordd
-- gr = "S -> abS\nS -> c"
-- wordd = "adab"


 