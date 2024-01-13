module Table where

import Parser
import Grammar
import LR0
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List as List

defaultNterminal :: String
defaultNterminal = "$"

data Action = None | Accept | Shift Int | Reduce GrammarRule 
    deriving (Eq, Ord, Show)

data Column = Actions (Map Int (Map String [Action])) | Goto (Map Int (Map String Int))
    deriving (Eq, Ord)

instance Show Column where
    show (Actions actionsColumn) = 
        (foldr (++) "" ["\nState" ++ (show stateIndex) ++
            (foldr (++) "" ["  \"" ++ term ++ "\": " ++ (show tableState) |(term, tableState) <- (Map.toList state)])
            | (stateIndex, state) <- (Map.toList actionsColumn)]) ++ "\n"
    show (Goto gotoColumn) = 
        (foldr (++) "" ["\nState" ++ (show stateIndex) ++ 
            (foldr (++) "" [ (if stateIdx >= -1 then "  \"" ++ term ++ "\": " ++ (show stateIdx) else " ") |(term, stateIdx) <- (Map.toList state)])
            | (stateIndex, state) <- (Map.toList gotoColumn)]) ++ "\n"


insert :: Ord k1 => Ord k2 => Map k1 (Map k2 a) -> k1 -> k2 -> a -> Map k1 (Map k2 a)
insert map k1 k2 value = (Map.insert k1 (Map.insert k2 value (map Map.! k1)) map)

add :: Ord k1 => Ord k2 => Map k1 (Map k2 [a]) -> k1 -> k2 -> a -> Map k1 (Map k2 [a])
add map k1 k2 value = (Map.insert k1 (Map.insertWith (++) k2 [value] (map Map.! k1)) map)

get :: Ord k1 => Ord k2 => Map k1 (Map k2 a) -> k1 -> k2 -> Maybe a
get map k1 k2 
    | (Map.member k1 map) && (Map.member k2 (map Map.! k1)) = Just ((map Map.! k1) Map.! k2)
    | otherwise = Nothing

delete :: Ord k1 => Ord k2 => Map k1 (Map k2 a) -> k1 -> k2 -> Map k1 (Map k2 a)
delete map k1 k2 = (Map.insert k1 (Map.delete k2 (map Map.! k1)) map)

data Table = Table {
    actionColumn :: Column,
    gotoColumn :: Column
} deriving (Eq, Ord, Show)

empty :: Table
empty = Table (Actions Map.empty) (Goto Map.empty)

make :: Grammar -> String -> [RulesDict] -> Table
make grammar startSymbol states = let
    addAction :: Column -> Int -> [(String, Set GrammarRule)] -> RulesDict -> Column
    addAction actions stateIndex [] state = actions
    addAction actions stateIndex ((nterminal, productions):stateItems) state = let
        addAction' :: Column -> [GrammarRule] -> Column
        addAction' actions [] = actions
        addAction' actions ((GrammarRule dotIndex nterminal product):productions) = 
            case actions of 
                Actions actions ->
                    if (dotIndex == (length product)) || ((product !! dotIndex) == "")
                        then 
                            if nterminal == startSymbol
                            then 
                                case actions Map.! stateIndex Map.! defaultNterminal of
                                [None] -> 
                                    addAction' (Actions (Table.insert actions stateIndex defaultNterminal [Accept]))
                                        productions
                            
                            else let
                                addAction'' :: Column -> [String] -> Column
                                addAction'' actions [] = actions
                                addAction'' actions (terminal:terminals) =
                                    case actions of
                                        Actions actions -> let
                                            action = Reduce (GrammarRule (-1) nterminal 
                                                        (if (List.length product) == dotIndex
                                                            then  product
                                                            else product
                                                        ))
                                            in case (Table.get actions stateIndex terminal) :: Maybe [Action] of
                                                Just [None] -> addAction'' (Actions (Table.insert actions stateIndex terminal [action])) terminals
                                                Nothing -> addAction'' (Actions (Table.insert actions stateIndex terminal [action])) terminals
                                                _ -> addAction'' (Actions (Table.add actions stateIndex terminal action)) terminals
                                in addAction' (addAction'' (Actions actions) (Set.toList (follow grammar nterminal Set.empty))) productions
                        else let
                            nextTerm = (product !! dotIndex)
                            nextStateIndex = List.elemIndex (goto grammar nextTerm state) states
                            action = case nextStateIndex of
                                Just index -> (Shift index)
                                _ -> None
                            in if (not . isNterminal) nextTerm
                                then case (Table.get actions stateIndex nextTerm) of
                                    Nothing -> addAction' (Actions (Table.insert actions stateIndex nextTerm [action])) productions
                                    Just actionState -> 
                                        if action `elem` actionState
                                            then addAction' (Actions actions) productions
                                            else addAction' (Actions (Table.add actions stateIndex nextTerm action)) productions
                                else addAction' (Actions actions) productions
        in addAction (addAction' actions (Set.toList productions)) stateIndex stateItems state
    makeActions :: Column -> Int -> [RulesDict] -> Column
    makeActions actions stateIndex [] = actions
    makeActions actions stateIndex (state:states) = 
        case actions of
            Actions actions -> makeActions 
                (addAction (Actions (Map.insert stateIndex (Map.singleton defaultNterminal [None]) actions)) stateIndex (Map.toList state) state)
                (stateIndex+1) states

    makeGoto :: Column -> Int -> [RulesDict] -> Column
    makeGoto gotoColumn stateIndex [] = gotoColumn
    makeGoto gotoColumn stateIndex (state:stateItems) = let
        makeGoto' :: Column -> [String] -> Column
        makeGoto' gotoColumn [] = gotoColumn
        makeGoto' gotoColumn (nterminal:nterminals) = let
            gotoResult = goto grammar nterminal state 
            nextStateIndex = List.elemIndex gotoResult states
            gotoState = case nextStateIndex of
                Just index -> index
            in case gotoColumn of
                Goto gotoMap -> 
                    if (Map.size gotoResult) == 0
                        then makeGoto' 
                            (Goto (Table.delete gotoMap stateIndex nterminal))
                            nterminals
                        else makeGoto' (Goto (Table.insert gotoMap stateIndex nterminal gotoState)) nterminals
                            
        in case gotoColumn of
            Goto gotoColumn -> makeGoto (makeGoto' (Goto 
                (Map.insert stateIndex Map.empty gotoColumn)) 
                (Set.toList (nterminals grammar)))
                (stateIndex+1) stateItems

    actions = (makeActions (Actions Map.empty) 0 states)
    gotoColumn = makeGoto (Goto Map.empty) 0 states
    in (Table actions gotoColumn)
