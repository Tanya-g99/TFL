module LR0Parser where

import Grammar
import LR0
import Table (Table)
import qualified Table as Table
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

data LR0Parser = NotLR0 String | LR0Parser {
    grammar :: Grammar,
    startSymbol :: String,
    states :: [RulesDict],
    table :: Table
} deriving (Eq, Ord)

instance Show LR0Parser where
    show (NotLR0 conflict) = "The grammar is not LR(0) because there is a " ++ conflict
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
    in case newTable of
        Table.Error err -> NotLR0 err
        newTable -> (LR0Parser grammar startSymbol states newTable)

-- Parse a string
data ParseResult = Yes | Not Int [String]
    deriving (Eq, Show)

parseString :: LR0Parser -> String -> ParseResult
parseString (NotLR0 err) string = error "Couldn't match expected constructor of data ‘LR0Parser’\nwith actual type ‘NotLR0’"
parseString (LR0Parser grammar startSymbol states (Table.Table (Table.Actions actions) (Table.Goto gotoColumn))) string = let
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
    parse ::  [Int] -> String -> Int -> ParseResult
    parse (state:stack) (c:string) index =
        case (Table.get actions state [c]) of
            Nothing -> Not index (Map.keys (states !! state))
            Just action -> 
                case action of
                    Table.None -> Not index (Map.keys (states !! state))
                    Table.Accept -> Yes
                    Table.Shift stateIndex -> parse (stateIndex:(state:stack)) string (index+1)
                    Table.Reduce rule -> 
                        parse 
                            (mutateStack (drop (productionLength (Grammar.product rule) 0) (state:stack)) (nterminal rule) gotoColumn) 
                            (c:string) index
    in parse [0] (string++"$") 0
