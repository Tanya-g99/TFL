module GraphStack where
import Grammar
import LR0Parser
import LR0 
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude  hiding (product)


data Node = Node {
    nodeId :: Int,
    state ::  RulesDict,
    term :: Maybe String,
    children :: [Node],
    parentId :: Maybe Int
} deriving (Eq)

instance Show Node where
    show (Node nodeId state term children parentId) =
        "\n" ++ "Node {nodeId = " ++ show nodeId  ++ ", parentId = " ++ show parentId ++ "}" ++ ", state = " ++ show state ++ ", term = " ++ show term ++
         ", children = " ++ show children 

initGraphStack :: Grammar -> Node
initGraphStack grammar = let
    makeStartSymbol :: String -> (Set String) -> String
    makeStartSymbol start nterminals = 
        if Set.member start nterminals 
            then makeStartSymbol (start ++ "'") nterminals
            else start
    startSymbol = makeStartSymbol (getInitNterminal grammar) (nterminals grammar)
    rootNode = Node 0 (closure grammar $ makeRulesDict [GrammarRule 0 startSymbol [getInitNterminal grammar]]) Nothing [] Nothing 
    rootNode' = createChildNodes grammar rootNode (Set.toList (alphabet grammar)) 1
    in rootNode' 


createChildNodes :: Grammar -> Node -> [String] -> Int -> Node
createChildNodes _ node [] _ = node
createChildNodes grammar parentNode (symbol:symbols) nextId =
    let
        newState = goto grammar symbol (state parentNode)
        childNode = if Map.null newState
            then Nothing
            else Just (Node { nodeId = nextId, state = newState, term = Just symbol, children = [], parentId = Just (nodeId parentNode) })
        
        newNextId = if Map.null newState
            then nextId
            else nextId + 1

        updatedChildren = case childNode of
            Just c -> (children parentNode) ++ [createChildNodes grammar c (Set.toList (alphabet grammar)) newNextId]
            Nothing -> children parentNode
        
        updatedNode = parentNode { children = updatedChildren }
    in
        createChildNodes grammar updatedNode symbols newNextId

-- createChildNodes :: Grammar -> Int -> Node -> [String] -> Node
-- createChildNodes _ _ node [] = node
-- createChildNodes grammar index parentNode (symbol:symbols) =
--     let
--         newState = goto grammar symbol (state parentNode)
--         childNode = if Map.null newState
--             then Nothing
--             else Just (Node { nodeId = index + 1, state = newState, term = Just symbol, children = [], parentId = Just (nodeId parentNode) })
--         updatedChildren = case childNode of
--             Just c -> children parentNode ++ [c]
--             Nothing -> children parentNode
--         updatedNode = parentNode { children = updatedChildren }
--     in
--         createChildNodes grammar (index + 1) updatedNode symbols

 

-- getLR0parser :: String -> LR0Parser
-- getLR0parser grammar = initLR0Parser $ initGrammar grammar

gr = initGrammar "S -> abS\nS -> c"
test = initGraphStack gr








 