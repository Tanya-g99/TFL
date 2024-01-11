module GraphStack where
import Grammar
import LR0 
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude  hiding (product)
import Control.Monad.State
 
data Node = Node {
    nodeId :: Int,
    stateRule ::  RulesDict,
    term :: Maybe String,
    children :: [Node],
    parentId :: Maybe Node
} deriving (Eq)

instance Show Node where
    show (Node nodeId state term children parentId) =
        "\n" ++ "Node {nodeId = " ++ show nodeId  ++ "}" ++ ", state = " ++ show state ++ ", term = " ++ show term ++
         ", children = " ++ show children ++ "\n" 

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

 

 

-- getLR0parser :: String -> LR0Parser
-- getLR0parser grammar = initLR0Parser $ initGrammar grammar

gr = initGrammar "S -> abS\nS -> c"
-- test = initGraphStack gr


-- :set -package mtl





 