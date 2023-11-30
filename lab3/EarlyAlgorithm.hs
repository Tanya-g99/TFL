module  EarlyAlgorithm where

import Data.List (nub)

import Data.Set (Set)
import qualified Data.Set as Set

 
import Data.Map (Map)
import qualified Data.Map as Map



EARLY_INIT_NTERM = "___"

-------------------------
data Situation = Situation { rule :: GrammarRule,
                            dotPos :: Int,
                            initIdx :: Int
                           } deriving ( Eq)
 
-- instance Eq Situation where
--     (==) (Situation rule1 dot_pos1 init_idx1) (Situation rule2 dot_pos2 init_idx2) =
--         rule1 == rule2 && dot_pos1 == dot_pos2 && init_idx1 == init_idx2

instance Show Situation where
    show (Situation rule dotPos initIdx) = "(" ++ nterm ++ " -> " ++ beforeDot ++ "." ++ afterDot ++ ", " ++ show initIdx ++ ")"
        where
            nterm = nterm rule
            (beforeDot, afterDot) = splitAt dotPos (product rule)

withMovedDot :: Situation -> Situation
withMovedDot situation = situation { dotPos = dotPos situation + 1 }

-- Helper function to check if a situation is the last situation in a rule
isDotLast :: Situation -> Bool
isDotLast (Situation rule dotPos _) = dotPos == length (product rule)

--to check if the next item in a situation is a Non-Terminal
isNextNterm :: Situation -> Bool
isNextNterm (Situation rule dotPos _) = not (isDotLast (Situation rule dotPos _)) && isNterm (product rule !! dotPos)
 
--to get the next item in a situation
getNextItem :: Situation -> String
getNextItem (Situation rule dotPos _) = product rule !! dotPos

-------------------------

-- class EarlyAlgorithm:
--     def __init__(self, grammar: Grammar):
--         self.grammar = grammar
--         self.rules_dict = self._gen_rules_dict(grammar.rules)
--         self.situations: List[Set[Situation]] = []

-- data EarlyAlgorithm = EarlyAlgorithm { 
--     grammar :: Grammar, 
--     rulesDict :: [(String, Set GrammarRule)], 
--     situations :: [Set Situation] 
--     }


----????
genRulesDict :: [GrammarRule] -> Map String (Set GrammarRule)
genRulesDict rules = foldr f Map.empty rules
  where
    f rule acc = Map.insertWith Set.union (nterm rule) (Set.singleton rule) acc

    -- def _gen_rules_dict(rules):
    --     rules_dict = defaultdict(set)
    --     for rule in rules:
    --         rules_dict[rule.nterm].add(rule)
    --     return rules_dict


-- function to generate the finite situation

-- getFiniteSituation :: Grammar -> Situation
-- getFiniteSituation (Grammar rules _ _) = Situation (GrammarRule EARLY_INIT_NTERM [initNterm]) 1 0
--   where
--     initNterm = getInitNterm (head [nterm | GrammarRule nterm _ <- rules])

    -- def _get_finite_situation(self):
    --     init_nterm = self.grammar.get_init_nterm()
    --     return Situation(GrammarRule(EARLY_INIT_NTERM, [init_nterm]), dot_pos=1, init_idx=0)



predict :: Int -> Grammar -> [Set Situation] -> [Set Situation]
predict j grammar situations = foldr f (situations !! j) (situations !! j)
  where
    f situation acc
        | not (isNextNterm (getNextItem situation)) = acc
        | otherwise = foldr g acc (Map.findWithDefault Set.empty (getNextItem situation) (rulesDict grammar))
    g newRule acc = Set.insert (Situation newRule j) acc
    -- def _predict(self, j):
    --     for situation in self.situations[j].copy():
    --         if not situation.is_next_nterm():
    --             continue
    --         for new_rule in self.rules_dict[situation.get_next_item()]:
    --             self.situations[j].add(Situation(new_rule, init_idx=j))


--функция для выполнения полного шага in the Early algorithm

complete :: Int -> [Set Situation] -> [Set Situation]
complete j situations = foldr f (situations !! j) (situations !! j)
  where
    f situation acc
        | not (isDotLast situation) = acc
        | otherwise = Set.foldr g acc (situations !! initIdx situation)
    g prevSit acc
        | isNextNterm prevSit && getNextItem prevSit == nterm (rule situation) =
            Set.insert (movedDot prevSit) acc
        | otherwise = acc



-- the scan step in the Early algorithm

--    def _scan(self, j, word):
--         if j == 0:
--             return
--         for situation in self.situations[j - 1]:
--             if situation.is_next_nterm() or situation.is_dot_last():
--                 continue
--             if word[j - 1] == situation.get_next_item():
--                 self.situations[j].add(situation.with_moved_dot())

scan :: Int -> String -> [Set Situation] -> [Set Situation]
scan j word situations
    | j == 0 = situations
    | otherwise = foldr f (situations !! j) (situations !! (j - 1))
  where
    f situation acc
        | isNextNterm situation || isDotLast situation = acc
        | word !! (j - 1) == getNextItem situation =
            Set.insert (movedDot situation) acc
        | otherwise = acc



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

--????
checkingWord :: String -> Grammar -> [Set Situation] -> [Set Situation]
checkingWord [] _ situations = situations
checkingWord (w:word) grammar situations = checkingWord word grammar newSituations
  where
    newSituations = scan (length (w:word)) (w:word) completedSituations
    completedSituations = complete (length (w:word)) predictedSituations
    predictedSituations = predict (length (w:word)) grammar situations

--DELETE
getInitNterm :: String -> String
getInitNterm (nterm:_) = if isNterm nterm then nterm else error "Invalid initial non-terminal"

 
-- def predict_word(self, word: str):
--         self.situations = [set() for i in range(len(word) + 1)]
--         init_situation = Situation(GrammarRule(EARLY_INIT_NTERM, [self.grammar.get_init_nterm()]),
--                                    dot_pos=0, init_idx=0)
--         self.situations[0].add(init_situation)
--         self._checking_word(word)
--         finite_rule = self._get_finite_situation()
--         return finite_rule in self.situations[len(word)]

-- getInitNterm - in module Grammar
predictWord :: String -> Grammar -> Bool
predictWord word grammar = finiteRule `Set.member` (situations !! length word)
  where
    situations = [Set.empty] ++ checkingWord word grammarSituations [initSituation]
    initSituation = Situation (GrammarRule EARLY_INIT_NTERM [getInitNterm grammar]) 0 0
    grammarSituations = replicate (length word + 1) Set.empty
    finiteRule = Situation (GrammarRule EARLY_INIT_NTERM [nterm initSituation]) 1 0