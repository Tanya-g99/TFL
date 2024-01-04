module LR0 where

import Grammar
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

appendDot :: GrammarRule -> GrammarRule
appendDot (GrammarRule dotIndex nterminal product) = (GrammarRule 0 nterminal product)

incrementDotIndex :: GrammarRule -> GrammarRule
incrementDotIndex (GrammarRule dotIndex nterminal product) = (GrammarRule (dotIndex+1) nterminal product)


searchTerm :: String -> GrammarRule -> Bool
searchTerm term (GrammarRule dotIndex nterminal product) = 
    (not (null (drop dotIndex product))) && 
    ((head (drop dotIndex product)) == term)

goto :: Grammar -> String -> RulesDict -> RulesDict
goto grammar symbol state = let
    itState = Map.toAscList state
    makeNewState'' :: String -> [GrammarRule] -> RulesDict -> RulesDict
    makeNewState'' nterminal [] newState = newState
    makeNewState'' nterminal (p:product) newState =
        makeNewState'' nterminal product 
        (if searchTerm symbol p
            then Map.insertWith (Set.union) nterminal (Set.fromList [incrementDotIndex p]) newState
            else newState)

    makeNewState' :: [(String, Set GrammarRule)] -> RulesDict -> RulesDict
    makeNewState' [] newState = newState
    makeNewState' ((nterminal, product):stateItems) newState = 
        makeNewState' stateItems (makeNewState'' nterminal (Set.toList product) newState)

    newState = makeNewState' itState Map.empty
    in closure grammar newState

nterminalAfterDot :: [Set GrammarRule] -> [String]
nterminalAfterDot rules = let
    firstNterminal :: [String] -> String
    firstNterminal [] = ""
    firstNterminal (elem:product)
        | isNterminal elem = elem
        | otherwise = ""
    nterminalAfterDot' :: [GrammarRule] -> [String] -> [String]
    nterminalAfterDot' [] res = res
    nterminalAfterDot' ((GrammarRule dotIndex _ product):rules) res = 
        nterminalAfterDot' rules ((firstNterminal (drop dotIndex product)):res)
    in nterminalAfterDot' (concat (map Set.toList rules)) []

makeNewState :: [GrammarRule] -> [Set GrammarRule] -> RulesDict -> RulesDict
makeNewState grammarRules rules state = let
    appendDerivations :: RulesDict -> String -> [GrammarRule] -> RulesDict
    appendDerivations newState nterminal [] = newState
    appendDerivations newState nterminal (derivation:derivations) = 
        if Set.member (appendDot derivation) (newState Map.! nterminal)
            then appendDerivations newState nterminal derivations
            else appendDerivations
                (Map.adjust (Set.insert (appendDot derivation)) nterminal newState) 
                nterminal derivations
    makeNewState' :: [String] -> RulesDict -> RulesDict
    makeNewState' [] newState = newState
    makeNewState' (nterminal:nterminals) newState = let
        in if null nterminal
            then makeNewState' nterminals newState
            else makeNewState' nterminals 
                (appendDerivations 
                (if (Map.notMember nterminal newState)
                    then (Map.insert nterminal Set.empty newState)
                    else newState)
                nterminal (filter (\(GrammarRule _ ruleNterminal _) -> ruleNterminal == nterminal) grammarRules))
    in makeNewState' (nterminalAfterDot rules) state

closure :: Grammar -> RulesDict -> RulesDict
closure grammar state = let
    productions = (Map.elems state)
    newState = makeNewState (rules grammar) productions state
    in if newState /= state
        then closure grammar newState
        else newState
