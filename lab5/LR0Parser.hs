module LR0Parser where

import GraphStack
import Grammar
import LR0
import Table (Table)
import qualified Table as Table
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

data LR0Parser = LR0Parser {
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

initLR0Parser :: Grammar -> LR0Parser
initLR0Parser grammar = let
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

    makeNewStates :: [RulesDict] -> [RulesDict]
    makeNewStates states = let
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
            then makeNewStates newStates
            else newStates

    newStates = makeNewStates states
    in (LR0Parser grammar startSymbol newStates table)

-- Table
initializeTable :: LR0Parser -> LR0Parser
initializeTable  (LR0Parser grammar startSymbol states table) = let
    newTable = Table.make grammar startSymbol states
    in (LR0Parser grammar startSymbol states newTable)

-- Parse a string
data ParseResult = Yes | Not Int [String]
    deriving (Eq, Show)

parseString :: LR0Parser -> String -> ParseResult
parseString (LR0Parser grammar startSymbol states (Table.Table (Table.Actions actions) (Table.Goto gotoColumn))) string = let
    makeMapString :: String -> Map Int String
    makeMapString string = let
        makeMapString' idx [] map = map
        makeMapString' idx (c:string) map = makeMapString' (idx+1) string (Map.insert idx [c] map)
        in makeMapString' 0 string Map.empty

    parse :: GraphStack -> [Node] -> Map Int String -> ParseResult
    parse stack (node:nodes) string = let
        applyActions :: [Table.Action] -> (Bool, GraphStack) -> GraphStack
        applyActions [] (pop, gs) = if pop
            then popTop node gs 
            else gs
        applyActions (action:actions) (pop, gs) = let
            shift :: Int -> GraphStack
            shift stateId = applyActions actions 
                    (pop, (push (Node stateId (index + 1)) term node gs))
            
            reduce :: String -> [String] -> GraphStack
            reduce nterminal product = let
                findNewTops :: Node -> [String] -> [Node]
                findNewTops node [] = [node]
                findNewTops node (term:product) = let
                    parents = (getParents node term gs)
                    newTops :: [Node] -> [Node]
                    newTops [] = []
                    newTops (n:nodes) = (findNewTops n product) ++ newTops nodes
                    in newTops parents
                addTops :: [Node] -> (Bool, GraphStack) -> (Bool, GraphStack)
                addTops [] gs = gs
                addTops (tnode:nodes) (pop, gs) =
                    case (Table.get gotoColumn (stateId tnode) nterminal) of
                        Just newStateId -> addTops nodes 
                            ((Node newStateId index) /= node, (push (Node newStateId index) nterminal tnode gs))
                        Nothing -> addTops nodes (pop, gs)
                in applyActions actions (addTops (findNewTops node (reverse product)) (True, gs))
            in case action of
                Table.Shift stateIdx -> shift stateIdx
                Table.Reduce (GrammarRule pop nterminal product) -> (reduce nterminal product)

        state = (stateId node)
        index = (wordState node)
        term = (string Map.! index)
        in case (Table.get actions state term) of
            Nothing -> Not index (Map.keys (states !! state))
            Just action -> 
                case action of
                    [Table.None] -> Not index (Map.keys (states !! state))
                    [Table.Accept] -> Yes
                    pop -> let
                        newStack = (applyActions action (True, stack))
                        in parse newStack (nodes ++ (listTopNodes newStack)) string
    in parse (initGraphStack) (listTopNodes (initGraphStack)) (makeMapString(string++"$"))

l = initLR0Parser (initGrammar "S->abc")
testl = parseString l "abc"
