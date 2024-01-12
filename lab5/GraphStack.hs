module GraphStack where
import Grammar
import LR0 
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude  hiding (product)
import Control.Monad.State 
import Data.List (find, nub)
import Data.Maybe (fromJust, isJust)

data Node = Node {
    nodeId :: Int,
    stateRule ::  RulesDict,
    term :: Maybe String,
    children :: [Node],
    parentId :: Maybe Node
} deriving (Eq)

instance Ord Node where
  compare node1 node2 = compare (stateRule node1) (stateRule node2)

instance Show Node where
    show (Node nodeId stateRule term children parentId) =
        "\n" ++ "Node {nodeId = "++   show nodeId  ++ "}" ++ ", state = " ++ show stateRule ++ ", term = " ++ show term ++
         ", children = " ++ show children ++ "\n" 


 
-- depthTraversal :: Node -> Int -> State (Set.Set Node) Node
-- depthTraversal node currentId = do
--     visited <- get
--     if Set.member node visited
--         then return node
--         else do
--             put (Set.insert node visited)
--             updatedChildren <- mapM (\(child, idx) -> depthTraversal child idx) (zip (children node) [currentId+1..])
--             return $ node { nodeId = currentId, children = updatedChildren }
 
depthTraversal :: Node -> Int -> State Int Node
depthTraversal node currentId = do
    updatedChildren <- mapM (\child -> do
        newId <- get
        put (newId + 1)
        depthTraversal child newId) (children node)
    return $ node { nodeId = currentId, children = updatedChildren }

initGraphStack :: Grammar -> String -> Node
initGraphStack grammar startSymbol = let
    rootNode = Node 0 (closure grammar $ makeRulesDict [GrammarRule 0 startSymbol [getInitNterminal grammar]]) Nothing [] Nothing 
    rootNode' = createChildNodes Set.empty grammar rootNode ((Set.toList (alphabet grammar) ++ (Set.toList (nterminals grammar)))) 1
    (rootNode'', _) = runState (depthTraversal rootNode' 1) 2 
    in rootNode''


createChildNodes :: Set RulesDict -> Grammar -> Node -> [String] -> Int -> Node
createChildNodes states _ node [] _ = node
createChildNodes states grammar parentNode (symbol:symbols) nextId =
    let
        newState = goto grammar symbol (stateRule parentNode)
        childNode = if (Map.null newState) || (Set.member newState states)
            then Nothing
            else Just (Node { nodeId = nextId, stateRule = newState, term = Just symbol, children = [], parentId = Just parentNode })
        
        newNextId = if (Map.null newState) || (Set.member newState states)
            then nextId
            else nextId + 1

        updatedChildren = case childNode of
            Just c -> (children parentNode) ++ [createChildNodes (Set.insert newState states) grammar c ((Set.toList (alphabet grammar))++ (Set.toList (nterminals grammar))) newNextId]
            Nothing -> children parentNode
        
        updatedNode = parentNode { children = updatedChildren }
    in
        createChildNodes (Set.insert newState states) grammar updatedNode symbols newNextId

 
getTerm :: Node -> Maybe String
getTerm node = term node

transition :: Node -> String -> Maybe Node
transition node term =
    case find (\child -> isJust (getTerm child) && fromJust (getTerm child) == term) (children node) of
        Just matchingChild -> Just matchingChild
        Nothing -> Nothing
 

getChildRules :: Node -> [RulesDict]
getChildRules node = map stateRule (children node)

getUniqueChildren :: Node -> [Node]
getUniqueChildren node =
    let descendants = getaAllChildren node
    in nub descendants

getaAllChildren :: Node -> [Node]
getaAllChildren node = 
    let childrenList = children node
        descendantsList = concatMap getaAllChildren childrenList
    in node : descendantsList

-- getLR0parser :: String -> LR0Parser
-- getLR0parser grammar = initLR0Parser $ initGrammar grammar

gr = initGrammar "S -> abS\nS -> c"
test = initGraphStack gr "S'"

testChild = getUniqueChildren test 

--ghci :set -package mtl





 