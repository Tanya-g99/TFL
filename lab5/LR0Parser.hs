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
    productionLength :: [String] -> Int -> Int
    productionLength [] length = length
    productionLength (term:production) length =
        if (null term)
            then productionLength production length
            else productionLength production (length+1)
    mutateStack :: [Int] -> String -> (Map Int (Map String Int)) -> [Int]
    mutateStack stack nterminal gotoColumn = case (Table.get gotoColumn (head stack) nterminal) of
        Just state -> (state:stack)
        Nothing -> stack
    parse :: GraphStack -> [Node] -> Map Int String -> ParseResult
    parse stack (node:nodes) string = let
        applyActions :: [Table.Action] -> GraphStack -> GraphStack
        applyActions [] gs = gs -- удалить из топов стека node => нужен (popTop node gs)
        applyActions (action:actions) gs = let
            shift :: Int -> GraphStack
            shift stateId = applyActions actions 
                    (push (Node stateId (index + 1) [] [Just node]) term gs)
            
            reduce :: String -> [String] -> GraphStack
            reduce nterminal product = let
                findNewTops :: Node -> [String] -> [Node]
                findNewTops node [] = [node]
                findNewTops node (term:product) = let
                    parents = getParents node term
                    newTops :: [Node] -> [Node]
                    newTops [] = []
                    newTops (n:nodes) = (findNewTops n product) ++ newTops nodes
                    in newTops parents
                addTops :: [Node] -> GraphStack -> GraphStack
                addTops (tnode:nodes) gs =
                    case (Table.get gotoColumn (stateId tnode) nterminal) of
                        Just newStateId -> addTops nodes 
                            (push (Node newStateId index [] [Just tnode]) nterminal gs)
                        Nothing -> addTops nodes gs
                in addTops (findNewTops node product) gs
            in case action of
                Table.Shift stateIdx -> shift stateIdx
                Table.Reduce (GrammarRule _ nterminal product) -> applyActions actions gs

        state = (stateId node)
        index = (wordState node)
        term = (string Map.! index)
        in case (Table.get actions state term) of
            Nothing -> Not index (Map.keys (states !! state))
            Just action -> 
                case action of
                    [Table.None] -> Not index (Map.keys (states !! state))
                    [Table.Accept] -> Yes
                    _ -> let
                        newStack = (applyActions action stack)
                        in parse newStack (nodes ++ (listTopNodes newStack)) string
    in parse (initGraphStack) (listTopNodes (initGraphStack)) (makeMapString(string++"$"))

