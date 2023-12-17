module Parser where

import Grammar
import Data.Set (Set)

import Prelude  hiding (product)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map


genRsDict ::  Grammar -> Map String (Set GrammarRule)
genRsDict grammar = Map.fromListWith Set.union $ concatMap (\rule -> [(nterminal rule, Set.singleton rule)]) (rules grammar) 


isStringInSet :: Eq a => a -> [a] -> Bool
isStringInSet _ [] = False
isStringInSet s (x:xs)
  | s == x = True
  | otherwise = isStringInSet s xs
    
makeFirst :: [String] -> Grammar -> Set String -> Set String     
makeFirst str grammar visited 
    | length(str) > 1  = firstString grammar str Set.empty visited "" 0
    | isNterminal (head str) && checkVisited = goFirst getRulesByN visited' Set.empty
    | isNterminal (head str) && (not checkVisited) = goFirst getRulesByN visited Set.empty
    | head str == "ϵ" = Set.singleton "ϵ"
    | otherwise = Set.singleton (head str) --nterm
 where
    ruleDic = genRsDict grammar
    checkVisited = isStringInSet (head str) $ Set.toList(visited)
    visited' = Set.insert (head str) visited
    getRulesByN = Set.toList(Map.findWithDefault (Set.empty) (head str) ruleDic)

    goFirst :: [GrammarRule] -> Set String -> Set String -> Set String  
    goFirst [] _ first = first
    goFirst (r:rules) visit first = goFirst rules visit $ firstString grammar (product r) Set.empty visit (head str) 0
 

firstString :: Grammar -> [String] -> Set String -> Set String -> String -> Int ->  Set String
firstString grammar str first visited symbol element
    | element >= length str = first
    | elemByInd == symbol || isStringInSet elemByInd (Set.toList(visited)) = first
    | isStringInSet "ϵ" (Set.toList(firstOfElement)) = 
        if element == length symbol 
        then firstString grammar str (Set.insert "ϵ" (first `Set.union` delEps)) visited symbol (element + 1)
        else firstString grammar str (first `Set.union` delEps) visited symbol (element + 1)
    | otherwise = first `Set.union` firstOfElement
  where
    elemByInd = str !! element
    firstOfElement = makeFirst [elemByInd] grammar visited 
    delEps = Set.fromList(deleteAtIndex (findIndex "ϵ" (Set.toList (firstOfElement))) (Set.toList (firstOfElement)))


--Если элемент найден - его индекс, а если нет: -1.
findIndex :: Eq a => a -> [a] -> Int
findIndex x list = go x list 0
  where
    go _ [] _ = -1
    go y (h:t) index
      | y == h = index
      | otherwise = go y t (index + 1)

--удаляет элемент из списка по индексу
deleteAtIndex :: Int -> [a] -> [a]
deleteAtIndex _ [] = []
deleteAtIndex n xs
  | n < 0 = xs
  | otherwise = let (h, t) = splitAt n xs
                in if length t > 0
                   then h ++ tail t
                   else h

listFromIndex :: Int -> [a] -> [a]
listFromIndex n xs
  | n < 0 = xs
  | otherwise = drop n xs


joinFirstFollow :: Set String -> Set String -> Set String
joinFirstFollow first follow 
                        | findIndex "ϵ" lst /= (-1) = follow `Set.union` Set.fromList(deleteAtIndex (findIndex "ϵ" lst) lst)
                        | otherwise = first `Set.union` follow
                    where 
                        lst = Set.toList(first)

addValueToSetInMap :: (Ord k, Ord v) => k -> v -> Map k (Set v) -> Map k (Set v)
addValueToSetInMap key value ls =
  Map.insertWith Set.union key (Set.singleton value) ls
 
 
followThird :: Grammar -> Map String (Set String)
followThird grammar = subsets
  where
    start = getInitNterminal grammar
    nonTerminals = Set.toList(nterminals grammar)
    productions = rules grammar
    dict = genRsDict grammar
    subsets' = Map.fromList [(start, Set.empty)] `Map.union` Map.fromList [(nt, Set.empty) | nt <- Set.toList ( nterminals grammar), nt /= start]
    subsets = process nonTerminals dict subsets'

    process :: [String] -> Map String (Set GrammarRule) -> Map String (Set String) -> Map String (Set String)
    process [] _ sub = sub
    process (n:nterms) dic sub = process nterms dic (go (Set.toList(Map.findWithDefault (Set.empty) n dic)) n sub)
    
    go :: [GrammarRule] -> String -> Map String (Set String) -> Map String (Set String)
    go [] _ sub = sub
    go (r:rs) nt sub | not (isNterminal (head (takePr r))) = go rs  nt sub
                     | isStringInSet "ϵ" (Set.toList(makeFirst (takePr r) grammar (Set.empty))) = 
        go ((GrammarRule {dotIndex = dotIndex r, nterminal = nterminal r, product = (deleteAtIndex (length (product r) - 1) (product r)) } ) : rs ) nt $ match nt r sub
                     | otherwise = go rs nt $ match nt r sub

    --if match.group(1) != non_terminal:
    match :: String -> GrammarRule -> Map String (Set String) -> Map String (Set String)
    match nt r sub | isNterminal (head (takePr r)) && (not (nt == head (takePr r))) = addValueToSetInMap (head (takePr r)) nt sub
                   | otherwise = sub
    takePr :: GrammarRule -> [String]
    takePr r = [(head (reverse (product r)))]
 
 

followOneTwo :: Grammar -> String -> Set String
followOneTwo grammar symbol = follow'
  where
    set = Set.empty

    follow = followCheckStart set
    --если символ - стартовый, добавляем $
    followCheckStart set 
            | (symbol == getInitNterminal grammar) =  Set.singleton "$"
            | otherwise = set
    
    follow' = followOneTwo' (rules grammar) follow
    
    followOneTwo' :: [GrammarRule] -> Set String -> Set String
    followOneTwo' [] acc = acc
    followOneTwo' (r:rs) acc =  followOneTwo' rs (followOneTwo'' (product r) acc)

    followOneTwo'' ::  [String] -> Set String -> Set String
    followOneTwo'' rule acc
                    | (findIndex symbol rule) /= (-1) = followOneTwo'' (deleteAtIndex(findIndex symbol rule) rule) $ followOneTwo''' (findIndex symbol rule) rule acc 
                    | otherwise = acc

    followOneTwo''' :: Int -> [String] -> Set String -> Set String
    followOneTwo''' index rule acc  
                    | index /= (length rule - 1) = joinFirstFollow (makeFirst (listFromIndex (index + 1) rule) grammar Set.empty) acc
                    | otherwise = acc
                    
   
follow :: Grammar -> String -> Set String -> Set String 
follow grammar symbol visited = 
  let 
    newVisited = Set.insert symbol visited
    followSet = followOneTwo grammar symbol
    follow''' = followThird grammar
    nterms = Set.toList(nterminals grammar)
    follow' = checkThirdRule nterms follow''' followSet

    checkThirdRule :: [String] -> Map String (Set String) -> Set String -> Set String
    checkThirdRule [] _ res = res
    checkThirdRule (n:nterms) dic res = checkThirdRule nterms dic (check n (Set.toList(Map.findWithDefault (Set.empty) n dic)) res)

    check :: String -> [String] -> Set String ->  Set String 
    check _ [] res = res
    check nt (sub:subs) res
                            | (symbol `elem` [sub]) && (nt `Set.notMember` newVisited) =
        res `Set.union` (follow grammar nt newVisited) 
                            | (symbol `elem` [sub]) = res `Set.union` (followOneTwo grammar nt)
                            | otherwise = res
   in follow'
 
 
g = initGrammar "S -> a S b|b S a|a BaB a|b A b| ϵ\nS -> S S|c\nA -> SaSaS\nBaB -> SbSbS"
test2 = follow g "S" Set.empty