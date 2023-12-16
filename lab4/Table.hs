module Table where

import Grammar
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

defaultNterminal :: String
defaultNterminal = "$"

data TableState = None | Accept | Shift Int | Reduce GrammarRule 
    deriving (Eq, Ord, Show)

data Column = Conflict String String | Actions (Map Int (Map String TableState)) | Goto (Map Int (Map String Int))
    deriving (Eq, Ord, Show)

insert :: Ord k1 => Ord k2 => Map k1 (Map k2 a) -> k1 -> k2 -> a -> Map k1 (Map k2 a)
insert map k1 k2 value = (Map.insert k1 (Map.insert k2 value (map Map.! k1)) map)

data Table = Error | Table {
    action :: Column,
    goto :: Column
} deriving (Eq, Ord, Show)

empty :: Table
empty = Table (Actions Map.empty) (Goto Map.empty)

make :: Grammar -> String -> [RulesDict] -> Table
make grammar startSymbol states = let
    addAction :: Column -> Int -> [(String, Set GrammarRule)] -> Column
    addAction actions stateIndex [] = actions
    addAction actions stateIndex ((nterminal, productions):state) = let
        addAction' :: Column -> [GrammarRule] -> Column
        addAction' actions [] = actions
        addAction' actions (production:productions) = 
            case actions of 
                Conflict state column -> actions
                Actions actions ->
                    if (Grammar.dotIndex production) == (length (Grammar.product production))
                        then 
                            if nterminal == startSymbol
                            then 
                                case actions Map.! stateIndex Map.! defaultNterminal of
                                None -> 
                                    addAction' (Actions (insert actions stateIndex defaultNterminal Accept))
                                        productions
                                _ -> Conflict (show state) defaultNterminal
                            
                            else addAction' (Actions actions) productions  -- follow 
                        else addAction' (Actions actions) productions
        in addAction (addAction' actions (Set.toList productions)) stateIndex state

    makeActions :: Column -> Int -> [RulesDict] -> Column
    makeActions actions stateIndex [] = actions
    makeActions actions stateIndex (state:states) = 
        case actions of
            Conflict state column -> actions
            Actions actions -> makeActions 
                (addAction (Actions (Map.insert stateIndex (Map.singleton defaultNterminal None) (actions))) stateIndex (Map.toList state))
                (stateIndex+1) states

    makeGoto :: Column -> Int -> [RulesDict] -> Grammar -> Column
    makeGoto goto stateIndex states grammar = goto

    actions = (makeActions (Actions Map.empty) 0 states)
    in case actions of 
        Conflict state column -> Error
        Actions _ -> (Table actions (makeGoto (Goto Map.empty) 0 states grammar))
