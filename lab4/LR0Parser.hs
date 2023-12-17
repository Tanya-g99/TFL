module LR0Parser where

import Grammar
import Table (Table)
import qualified Table as Table
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

data LR0Parser = NotLR0 | LR0Parser {
    grammar :: Grammar,
    startSymbol :: String,
    states :: [RulesDict],
    table :: Table
} deriving (Eq, Ord)

instance Show LR0Parser where
    show (LR0Parser grammar startSymbol states table) = let 
            show_states = ["\nState\n" ++ (show state) | state <- states]
        in (show grammar) ++ 
            "\nСтартовый символ: " ++ startSymbol ++ 
            (unwords show_states) ++ "\n" ++
            (show table)

appendDot :: GrammarRule -> GrammarRule
appendDot (GrammarRule dotIndex nterminal product) = (GrammarRule 0 nterminal product)

incrementDotIndex :: GrammarRule -> GrammarRule
incrementDotIndex (GrammarRule dotIndex nterminal product) = (GrammarRule (dotIndex+1) nterminal product)

initLR0Parser :: String -> LR0Parser
initLR0Parser str = let
    grammar = initGrammar str
    makeStartSymbol :: String -> (Set String) -> String
    makeStartSymbol start nterminals = 
        if Set.member start nterminals 
            then makeStartSymbol (start ++ "'") nterminals
            else start
    startSymbol = makeStartSymbol (getInitNterminal grammar) (nterminals grammar)
    states = [closure grammar $ makeRulesDict [GrammarRule 0 startSymbol [getInitNterminal grammar]]]
    in initializeTable (initializeStates (LR0Parser grammar startSymbol states Table.empty))

-- States
initializeStates :: LR0Parser -> LR0Parser
initializeStates  (LR0Parser grammar startSymbol states table) = let
    newStates = makeNewStates grammar states
    in (LR0Parser grammar startSymbol newStates table)

makeNewStates :: Grammar -> [RulesDict] -> [RulesDict]
makeNewStates grammar states = let
    stateToNewStates :: RulesDict -> [String] -> Set RulesDict -> [RulesDict]
    stateToNewStates state [] newStates = Set.toList newStates
    stateToNewStates state (term:terms) newStates = let
        newState = goto grammar term state
        in if null newState
            then stateToNewStates state terms newStates
            else stateToNewStates state terms 
                (Set.insert newState newStates)
    makeNewStates' :: [RulesDict] -> [RulesDict] -> [RulesDict]
    makeNewStates' [] newStates = newStates
    makeNewStates' (state:states) newStates = 
        makeNewStates' states (newStates ++ 
            (stateToNewStates state ((Set.toList (nterminals grammar)) ++ (Set.toList (alphabet grammar))) (Set.fromList newStates)))
    
    newStates = (Set.toList (Set.fromList (makeNewStates' states states)))
    in if (length states) /= (length newStates)
        then makeNewStates grammar newStates
        else newStates

nterminalAfterDot :: [Set GrammarRule] -> [String]
nterminalAfterDot rules = let
    firstNterminal :: [String] -> String
    firstNterminal [] = ""
    firstNterminal (elem:product)
        | isNterminal elem = elem
        | otherwise = firstNterminal product
    nterminalAfterDot' :: [GrammarRule] -> [String] -> [String]
    nterminalAfterDot' [] res = res
    nterminalAfterDot' ((GrammarRule dotIndex _ product):rules) res = 
        nterminalAfterDot' rules ((firstNterminal (drop dotIndex product)):res)
    in nterminalAfterDot' (concat (map Set.toList rules)) []

makeNewState :: [GrammarRule] -> [Set GrammarRule] -> RulesDict -> RulesDict
makeNewState grammarRules rules state = let
    appendDerivations :: RulesDict -> String -> [GrammarRule] -> RulesDict
    appendDerivations state nterminal [] = state
    appendDerivations state nterminal (derivation:derivations) = 
        if Set.member (appendDot derivation) (state Map.! nterminal) 
            then appendDerivations state nterminal derivations
            else appendDerivations 
                (Map.adjust (Set.insert (appendDot derivation)) nterminal state) 
                nterminal derivations
    makeNewState' :: [String] -> RulesDict -> RulesDict
    makeNewState' [] newState = newState
    makeNewState' (nterminal:nterminals) newState = let
        in if null nterminal
            then makeNewState' nterminals newState
            else makeNewState' nterminals 
                (appendDerivations 
                (if (Map.notMember nterminal state)
                    then (Map.insert nterminal Set.empty state)
                    else state)
                nterminal grammarRules)
    in makeNewState' (nterminalAfterDot rules) state

closure :: Grammar -> RulesDict -> RulesDict
closure grammar state = let
    productions = (Map.elems state)
    newState = makeNewState (rules grammar) productions state
    in newState

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

-- Table
initializeTable :: LR0Parser -> LR0Parser
initializeTable  (LR0Parser grammar startSymbol states table) = let
    newTable = Table.make grammar startSymbol states
    in case newTable of
        Table.Error -> NotLR0
        newTable -> (LR0Parser grammar startSymbol states newTable)
