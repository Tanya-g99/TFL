module EarlyAlgorithm where
import Grammar
import Data.List hiding (product, lookup)
import Prelude  hiding (product)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map
 
-------------------------
data Situation = Situation {
     rule :: GrammarRule,
     dotPos :: Int,
     initIdx :: Int
     }  
     
data EarlyAlgorithm = EarlyAlgorithm { 
    grammar :: Grammar, 
    rulesDict :: RulesDict, 
    situations :: [Set Situation]
    } 
 
instance Eq Situation where
    (==) (Situation rule1 dot_pos1 init_idx1) (Situation rule2 dot_pos2 init_idx2) =
        rule1 == rule2 && dot_pos1 == dot_pos2 && init_idx1 == init_idx2

instance Show Situation where
    show (Situation rule dotpos initIdx) =
        "(" ++ nterminal rule ++ " -> " ++ beforeDot ++ "." ++ afterDot ++ ", " ++ show initIdx ++ ")" ++ "\n"
            where
                beforeDot = unwords $ take dotpos $ product rule
                afterDot = unwords $ drop dotpos $ product rule

instance Ord Situation where
    compare (Situation rule1 dotPos1 initIdx1) (Situation rule2 dotPos2 initIdx2) =
        compare (rule1, dotPos1, initIdx1) (rule2, dotPos2, initIdx2)

withMovedDot :: Situation -> Situation
withMovedDot (Situation rule dpos initidx) = Situation rule (dpos + 1) initidx

 
isDotLast :: Situation -> Bool
isDotLast (Situation rule dotPos _) = dotPos == length (product rule)

isNextNterm :: Situation -> Bool
isNextNterm (Situation rule dotPos initIdx) = not (isDotLast (Situation rule dotPos initIdx)) && isNterminal (product rule !! dotPos)
 
getNextItem :: Situation -> String
getNextItem (Situation rule dotPos _) = (product rule) !! dotPos

-------------------------

-- function to generate the finite situation
getFiniteSituation :: Grammar -> Situation
getFiniteSituation grammar = Situation GrammarRule {dotIndex = 0, nterminal = "___", product = [getInitNterminal grammar]} 1 0
 
predict :: EarlyAlgorithm -> Int -> EarlyAlgorithm
predict alg j = alg { situations = updateSetAtIndex (situations alg) newElem' j}
    where 
        newElem = addSituation lst lst
        lst = Set.toList (situations alg !! j)
        newElem' = Set.fromList newElem
         
        addSituation :: [Situation] -> [Situation] -> [Situation]
        addSituation [] set = set
        addSituation (situation:xs) set
            | not(isNextNterm situation) = addSituation xs set
            | otherwise = addSituation xs (addNewRule (convert situation) set)
        
        convert sit = Set.toList $ Map.findWithDefault (Set.empty) (getNextItem sit) (rulesDict alg)
        addNewRule :: [GrammarRule] -> [Situation] -> [Situation]
        addNewRule [] set = set
        addNewRule (newRule:xs) set = addNewRule xs $ (Situation newRule 0 j) : set
          
 
complete :: EarlyAlgorithm -> Int -> EarlyAlgorithm
complete alg j = alg { situations = updateSetAtIndex (situations alg) newElem' j}
    where 
        newElem = addSituation lst lst
        lst = Set.toList (situations alg !! j)
        newElem' = Set.fromList newElem
         
        addSituation :: [Situation] -> [Situation] -> [Situation]
        addSituation [] set = set
        addSituation (situation:xs) set
            | not(isDotLast situation) = addSituation xs set
            | otherwise = addSituation xs $ addPrevSituation (Set.toList (situations alg !! (initIdx situation))) situation set
        
        addPrevSituation:: [Situation] -> Situation -> [Situation] -> [Situation]
        addPrevSituation [] _ set = set
        addPrevSituation (prevSit:rs) situation set
            | isNextNterm prevSit && getNextItem prevSit == nterminal (rule situation) = addPrevSituation rs situation ((withMovedDot prevSit) : set)
            | otherwise = addPrevSituation rs situation set

scan :: Int -> String -> EarlyAlgorithm -> EarlyAlgorithm
scan  j word alg
    | j == 0 = alg
    | otherwise = alg { situations = updateSetAtIndex (situations alg) newElem' j}
        where 
            newElem = addSituation lst lst'
            lst' = Set.toList (situations alg !! j)
            lst = Set.toList (situations alg !! (j - 1))
            newElem' = Set.fromList newElem
            addSituation :: [Situation] -> [Situation] -> [Situation]
            addSituation [] set = set
            addSituation (situation:xs) set
                | isNextNterm situation || isDotLast situation = addSituation xs set
                | [word !! (j - 1)] == getNextItem situation =  addSituation xs $ (withMovedDot situation) : set
                | otherwise = addSituation xs set


loop :: EarlyAlgorithm -> Int -> Int -> EarlyAlgorithm
loop alg setLength j = if length (situations alg !! j) == setLength then alg else loop (predict (complete alg j) j) (length (situations alg !! j)) j 

checkingWord :: String -> Int -> Int ->  EarlyAlgorithm ->  EarlyAlgorithm
checkingWord word j lenW algo 
  | j > lenW = algo
  | otherwise = checkingWord word j2 lenW newAlgo
   where 
    newAlgo' = scan j word algo
    last_set_len = length $ situations newAlgo' !! j
    newAlgo = loop (predict (complete newAlgo' j) j) last_set_len j
    j2 = j + 1

updateSetAtIndex :: [a] -> a -> Int -> [a]
updateSetAtIndex list newElem index =
  take index list ++ [newElem] ++ drop (index + 1) list


earlyAlgorithm :: Grammar -> String -> Int
earlyAlgorithm gr word =
  let rulesDict = genRulesDict gr

      emptySituations = replicate (length word + 1) Set.empty
      initSituation = Situation GrammarRule {dotIndex = 0, nterminal = "___", product = [getInitNterminal gr]} 0 0
      firstSet = Set.singleton initSituation
      startSituations = updateSetAtIndex emptySituations firstSet 0

      algo = EarlyAlgorithm {grammar = gr, rulesDict = rulesDict, situations = startSituations}
      
      finalSits = checkingWord word 0 (length word) algo
 
      finalSit = getFiniteSituation gr
      check = finalSit `Set.member` last (situations finalSits)
      index = findErrorIndex finalSits check
  in  index
--   in finalSit `Set.member` last (situations finalSits)

findErrorIndex :: EarlyAlgorithm -> Bool -> Int
findErrorIndex earlyAlg check = result
    where 
    sits = situations earlyAlg
    result = getIndex sits (-1)
    getIndex :: [Set Situation] -> Int -> Int
    getIndex [] num | not check = num - 1
                    | otherwise = (-1)           --- слово принадлежит грамматике

    getIndex (s:ss) num   | Set.null s = num
                          | otherwise = getIndex ss (num + 1) 



earlyAlgorithm2 :: Grammar -> String -> String -> Int
earlyAlgorithm2 gr start word =
  let rulesDict = genRulesDict gr

      emptySituations = replicate (length word + 1) Set.empty
      initSituation = Situation GrammarRule {dotIndex = 0, nterminal = "___", product = [start]} 0 0
      firstSet = Set.singleton initSituation
      startSituations = updateSetAtIndex emptySituations firstSet 0

      algo = EarlyAlgorithm {grammar = gr, rulesDict = rulesDict, situations = startSituations}
      finalSits = checkingWord word 0 (length word) algo
 
      finalSit = Situation GrammarRule {dotIndex = 0, nterminal = "___", product = [start]} 1 0
      check = finalSit `Set.member` last (situations finalSits)
      index = checkInfix finalSits check
  in index
    -- in situations finalSits

checkInfix :: EarlyAlgorithm -> Bool -> Int
checkInfix earlyAlg check | check = 1 -- является суффиксом 
                          | Set.null (last (situations earlyAlg)) = 0  -- не является элементом языка
                          | otherwise = 2 -- является точным инфиксом
 

-- g = initGrammar "S -> a S b|b S a|a BaB a|b A b| ϵ\nS -> S S|c\nA -> SaSaS\nBaB -> SbSbS" 
g = initGrammar "S -> a A | C \nA -> a b | A A | C\nC -> c"
--"S -> a S b|b S a|a B a|b A b| ϵ\nS -> S S|c\nA -> SaSaS\nB -> SbSbS"
-- g = Grammar {rules = [GrammarRule {nterm = "S", product = ["(","S",")"]},GrammarRule {nterm = "S", product = ["S","S"]},GrammarRule {nterm = "S", product = ["a"]},GrammarRule {nterm = "S", product = ["$"]}], nterms = Set.fromList ["S"]}
wo = "ababa"
test = earlyAlgorithm2 g "A" wo
--  "S -> (S) | S S\nS -> a | $"