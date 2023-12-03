module EarlyAlgorithm where
import Grammar
import Data.List hiding (product, lookup)
import Prelude  hiding (product)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map
 
-- EARLY_INIT_NTERM = "___"
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
        "(" ++ nterm rule ++ " -> " ++ beforeDot ++ "." ++ afterDot ++ ", " ++ show initIdx ++ ")"
            where
                beforeDot = unwords $ take dotpos $ product rule
                afterDot = unwords $ drop dotpos $ product rule
instance Ord Situation where
    compare (Situation rule1 dotPos1 initIdx1) (Situation rule2 dotPos2 initIdx2) =
        compare (rule1, dotPos1, initIdx1) (rule2, dotPos2, initIdx2)

withMovedDot :: Situation -> Situation
withMovedDot (Situation rule dpos initidx) = Situation rule (dpos + 1) initidx


-- Helper function to check if a situation is the last situation in a rule
isDotLast :: Situation -> Bool
isDotLast (Situation rule dotPos _) = dotPos == length (product rule)

--to check if the next item in a situation is a Non-Terminal
isNextNterm :: Situation -> Bool
isNextNterm (Situation rule dotPos initIdx) = not (isDotLast (Situation rule dotPos initIdx)) && isNterm (product rule !! dotPos)
 
--to get the next item in a situation
getNextItem :: Situation -> String
getNextItem (Situation rule dotPos _) = (product rule) !! dotPos

-------------------------

type RulesDict = Map String (Set GrammarRule)
 
genRulesDict ::  Grammar -> RulesDict
genRulesDict grammar = Map.fromListWith Set.union $ concatMap (\rule -> [(nterm rule, Set.singleton rule)]) (rules grammar)

-- function to generate the finite situation
getFiniteSituation :: Grammar -> Situation
getFiniteSituation grammar = Situation (GrammarRule "___" [getInitNterm grammar]) 1 0
 
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
            | isNextNterm prevSit && getNextItem prevSit == nterm (rule situation) = addPrevSituation rs situation ((withMovedDot prevSit) : set)
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


--    def _scan(self, j, word):
--         if j == 0:
--             return
--         for situation in self.situations[j - 1]:
--             if situation.is_next_nterm() or situation.is_dot_last():
--                 continue
--             if word[j - 1] == situation.get_next_item():
--                 self.situations[j].add(situation.with_moved_dot())     

--    def _checking_word(self, word: str):
--         for j in range(0, len(word) + 1):
--             self._scan(j, word)
--             last_set_len = len(self.situations[j])
--             while True:
--                 self._complete(j)
--                 self._predict(j)
--                 if len(self.situations[j]) == last_set_len:
--                     break
--                 last_set_len = len(self.situations[j])

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


earlyAlgorithm :: Grammar -> String -> Bool
earlyAlgorithm gr word =
  let rulesDict = genRulesDict gr

      emptySituations = replicate (length word + 1) Set.empty
      initSituation = Situation (GrammarRule "___" [getInitNterm gr]) 0 0
      firstSet = Set.singleton initSituation
      startSituations = updateSetAtIndex emptySituations firstSet 0

      algo = EarlyAlgorithm {grammar = gr, rulesDict = rulesDict, situations = startSituations}
      
      finalSits = checkingWord word 0 (length word) algo
      g = (situations finalSits)
      finalSit = getFiniteSituation gr

  in finalSit `Set.member` last (situations finalSits)


g = Grammar {rules = [GrammarRule {nterm = "S", product = ["(","S",")"]},GrammarRule {nterm = "S", product = ["S","S"]},GrammarRule {nterm = "S", product = ["a"]},GrammarRule {nterm = "S", product = ["$"]}], nterms = Set.fromList ["S"]}
wo = "aaa"
test = earlyAlgorithm g wo
--  "S -> (S) | S S\nS -> a | $"
