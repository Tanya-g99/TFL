module FSM where

import Normalization

-- FSM = (states, start, finals, LTS)  
type FSM a = ([a], a, [a], [[a]])

type Transition = (Int, Int, Char) 
type MaybeTransition = (Maybe Int, Maybe Int, Char)

regexToRegExp :: String -> RegExp
regexToRegExp regex = toRegExp $ toPostfix $ insertDot regex

getNormDerivative :: Char -> RegExp -> RegExp
getNormDerivative char regex = simp $ deriv char regex

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

indexState :: RegExp -> [RegExp] -> Maybe Int
indexState re states = let
    indexState' re [] index = Nothing
    indexState' re (state:states) index 
        | re == state = Just index
        | otherwise = indexState' re states (index + 1)
    in indexState' re states 0

getTransitions :: [RegExp] -> [RegExp] -> String -> [(MaybeTransition, RegExp)]
getTransitions states regexs alphabet = let
    getTransition re dRe char states = (((indexState re states),
                                        (indexState dRe states),
                                        char), dRe)
    getTransitionsDers alphabet states re =
        [getTransition re (getNormDerivative char re) char states | char <- alphabet]
    
    in foldl (++) [] (map (getTransitionsDers alphabet states) regexs)

removeFoundStates :: Int -> [RegExp] -> [RegExp] -> [Transition] -> [(MaybeTransition, RegExp)] -> ([RegExp], [RegExp], [Transition])
removeFoundStates countStates newRegexs states transitions [] = (newRegexs, states, transitions)
removeFoundStates countStates regexs states transitions newTransitionsStates = let
    newTS:otherNewTS = newTransitionsStates
    (newT, newS) = newTS
    in case newT of
        (Just iq1, Just iq2, char) -> 
            removeFoundStates countStates regexs states ((iq1, iq2, char):transitions) otherNewTS
        (Just iq1, Nothing, char) -> 
            case newS of
                Zero -> removeFoundStates countStates regexs states transitions otherNewTS
                -- Eps -> removeFoundStates countStates regexs states ((iq1, -1, char):transitions) otherNewTS
                _ -> case (indexState newS states) of
                    Just iq -> removeFoundStates countStates regexs states ((iq1, iq, char):transitions) otherNewTS
                    Nothing -> removeFoundStates (countStates+1) (newS:regexs) (states ++ [newS]) ((iq1, countStates, char):transitions) otherNewTS

getTransitionsStates :: [RegExp] -> [Transition] -> [RegExp] -> String -> ([RegExp], [RegExp], [Transition])
getTransitionsStates states transitions regexs alphabet = 
    removeFoundStates (length states) [] states transitions (getTransitions states regexs alphabet)

-- По регулярке строит массив переходов transitions и массив состояний states
-- По переходам надо построить систему переходов LTS 
-- Из состояний нужно выделить стартовое(находится под индексом 0) 
-- и финальные  [state | state <- states, nullable state]
getAllTransitionsStates :: String -> ([Transition], [RegExp])
getAllTransitionsStates regex = let
    alphabet = getAlphabet regex
    re = regexToRegExp regex 
    getAllTransitionsStates' states transitions [] alphabet = (transitions, states)
    getAllTransitionsStates' states transitions regexs alphabet = let
        (newRegexs, newStates, newTransitions) = 
            getTransitionsStates states transitions regexs alphabet
        in getAllTransitionsStates' newStates newTransitions newRegexs alphabet
    in getAllTransitionsStates' [re] [] [re] alphabet

regExpToString :: RegExp -> String
regExpToString re = 
    parse [re] "" where
        parse :: [RegExp] -> String -> String
        parse re xs = case re of 
            [] -> xs
            Zero:rs -> parse rs ('∅':xs)
            Eps:rs -> parse rs ('ϵ':xs)
            [Union rs] -> '(' : tail (foldl (++) "" ["|" ++ (parse [ri] "") | ri <- rs]) ++ ")"
            [Shuf rs] -> '(' : tail (foldl (++) "" ["#" ++ (parse [ri] "") | ri <- rs]) ++ ")"
            [Cat rs] -> (foldl (++) "" [(parse [ri] "") | ri <- rs])
            Star r:rs -> (parse (r:rs) xs) ++ "*"
            Let x:rs -> parse rs (x:xs)

finalStates :: [RegExp] -> [RegExp]
finalStates states = [state | state <- states, nullable state]

startState :: [RegExp] -> RegExp
startState (start:re) = start

makeLTS :: Int -> [Transition] -> [[RegExp]] -- Дописать (сортировка пеерходов? и заполнение матрицы)
makeLTS countStates transitions = [[]]

makeFSM :: String -> FSM RegExp
makeFSM regex = let
    (transitions, states) = getAllTransitionsStates regex
    start = startState states
    finals = finalStates states
    lts = makeLTS (length states) transitions
    in (states, start, finals, lts)

test :: ([Transition], [String])
test = let
            (t, s) = getAllTransitionsStates "(a|b)*a(a|b)"
            nS = map (regExpToString) s
    in (t, nS)

