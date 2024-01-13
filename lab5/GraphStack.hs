module GraphStack where
import Grammar
import LR0 
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude  hiding (product)
import Data.List (find, nub)
import Data.Maybe (fromJust, isJust)


data GraphStack = GraphStack {
    listTopNodes :: [Node],
    stackNodes :: Node
}

data Node = Node {
    stateId :: Int,
    wordState :: Int,
    children :: [(String, Node)],
    parentNode :: [Maybe Node]
} deriving (Eq)

instance Show Node where
    show (Node stateId wordState children parentNode) =
        "Node { stateId: " ++ show stateId ++
        ", wordState: " ++ show wordState ++
        ", children: " ++ show children ++
        ", parentNode: " ++ show parentNode ++
        " }"

instance Ord Node where
  compare node1 node2 = compare (stateId node1, wordState node1) (stateId node2, wordState node2)

 
-- push :: Node -> String -> GraphStack -> GraphStack
-- push newNode string (GraphStack topNodes stackNode) =
--     case parentNode newNode of
--         Nothing -> GraphStack (newNode : topNodes) newNode
--         Just parent -> GraphStack updatedTopNodes updatedStackNode
--             where
--                 updatedStackNode = stackNode { children = (string, newNode) : children stackNode }
--                 updatedTopNodes = replaceParentNode newNode (Just parent) topNodes 

push :: Node -> String -> GraphStack -> GraphStack
push newNode string (GraphStack topNodes stackNode) =
    case findSameNode newNode stackNode of
        Just existingNode -> 
            let updatedStackNode = addToExistingNode newNode string existingNode stackNode
                updatedTopNodes = replaceParentNode newNode (head (parentNode newNode)) topNodes
            in GraphStack updatedTopNodes updatedStackNode
        Nothing ->
            let updatedStackNode = stackNode {children = (string, newNode) : children stackNode}
                updatedTopNodes = replaceParentNode newNode (head (parentNode newNode)) topNodes
            in GraphStack (newNode : topNodes) updatedStackNode


addToExistingNode :: Node -> String -> Node -> Node -> Node
addToExistingNode newNode string existingNode currentNode =
    if existingNode == currentNode
        then existingNode {parentNode = ((parentNode newNode) ++ parentNode existingNode)}
        else currentNode {children = map (\(key, node) -> if node == existingNode then (key, addToExistingNode newNode string existingNode node) else (key, node)) (children currentNode)}

findSameNode :: Node -> Node -> Maybe Node
findSameNode newNode currentNode =
    if newNode == currentNode
        then Just currentNode
        else case find (\(_, node) -> findSameNode newNode node /= Nothing) (children currentNode) of
            Just (_, foundNode) -> findSameNode newNode foundNode
            Nothing -> Nothing
 

replaceParentNode :: Node ->  Maybe Node -> [Node] -> [Node]
replaceParentNode _ _ [] = []
replaceParentNode newNode maybeParentNode topNodes = 
    case findNodeIndex parentNode topNodes of
        Just index -> let (front, _ : rest) = splitAt index topNodes
                      in front ++ [newNode] ++ rest
        Nothing -> topNodes ++ [newNode]
  where parentNode = fromJust maybeParentNode


initGraphStack :: GraphStack
initGraphStack = GraphStack [emptyNode] emptyNode
  where emptyNode = Node 0 0 [] [Nothing]


findNodeIndex :: Node -> [Node] -> Maybe Int
findNodeIndex _ [] = Nothing
findNodeIndex node nodeList = findIndexHelper node nodeList 0
  where
    findIndexHelper :: Node -> [Node] -> Int -> Maybe Int
    findIndexHelper _ [] _ = Nothing
    findIndexHelper targetNode (x:xs) currentIndex
      | targetNode == x = Just currentIndex
      | otherwise = findIndexHelper targetNode xs (currentIndex + 1)


-- deleteNodeAtIndex :: Int -> [Node] -> [Node]
-- deleteNodeAtIndex _ [] = []
-- deleteNodeAtIndex index nodeList
--   | index < 0 || index >= length nodeList = nodeList  -- если индекс находится за пределами списка, возвращаем исходный список
--   | otherwise = front ++ (if null rest then [] else tail rest)
--     where (front, rest) = splitAt index nodeList








-- instance Show Node where
--     show (Node   stateRule term children parentId) =
--         "\n"  ++ "state = " ++ show stateRule ++ ", term = " ++ show term ++
--          ", children = " ++ show children ++ "\n" 


 
-- -- depthTraversal :: Node -> Int -> State (Set.Set Node) Node
-- -- depthTraversal node currentId = do
-- --     visited <- get
-- --     if Set.member node visited
-- --         then return node
-- --         else do
-- --             put (Set.insert node visited)
-- --             updatedChildren <- mapM (\(child, idx) -> depthTraversal child idx) (zip (children node) [currentId+1..])
-- --             return $ node { nodeId = currentId, children = updatedChildren }
 
-- -- depthTraversal :: Node -> Int -> State Int Node
-- -- depthTraversal node currentId = do
-- --     updatedChildren <- mapM (\child -> do
-- --         newId <- get
-- --         put (newId + 1)
-- --         depthTraversal child newId) (children node)
-- --     return $ node { nodeId = currentId, children = updatedChildren }

-- initGraphStack :: Grammar -> String -> Node
-- initGraphStack grammar startSymbol = let
--     rootNode = Node (closure grammar $ makeRulesDict [GrammarRule 0 startSymbol [getInitNterminal grammar]]) Nothing [] Nothing 
--     rootNode' = createChildNodes grammar rootNode ((Set.toList (alphabet grammar) ++ (Set.toList (nterminals grammar)))) 
--     -- (rootNode'', _) = runState (depthTraversal rootNode' 1) 2 
--     in rootNode'


-- addChildIfExists :: String -> Node -> [(String, Node)] -> [(String, Node)]
-- addChildIfExists symbol childNode [] = [(symbol, childNode)]
-- addChildIfExists symbol childNode ((s, n):rest)
--     | stateRule n == stateRule childNode = (s, childNode) : rest
--     | otherwise = (s, n) : addChildIfExists symbol childNode rest

-- createChildNodes :: Grammar -> Node -> [String] -> Node
-- createChildNodes _ node [] = node
-- createChildNodes grammar parentNode (symbol:symbols) =
--     let
--         newState = goto grammar symbol (stateRule parentNode)
--         (childNode, remainingSymbols) =
--             if Map.null newState
--                 then (Nothing, symbols)
--                 else (Just (Node { stateRule = newState, term = Just symbol, children = [], parentId = Just parentNode }), symbols)

--         updatedChildren =
--             case childNode of
--                 Just c -> addChildIfExists symbol c (children parentNode)
--                 Nothing -> children parentNode

--         updatedNode = parentNode { children = updatedChildren }
--     in
--         case childNode of
--             Just c -> createChildNodes grammar c remainingSymbols  -- Рекурсия для добавления остальных символов к новому ребенку
--             Nothing -> createChildNodes grammar updatedNode remainingSymbols  -- Рекурсия для добавления остальных символов к существующему узлу
-- createChildNodes ::  Grammar -> Node -> [String] -> Node
-- createChildNodes _ node [] = node
-- createChildNodes grammar parentNode (symbol:symbols) =
--     let
--         newState = goto grammar symbol (stateRule parentNode)
--         (childNode, remainingSymbols) =
--             if Map.null newState
--                 then (Nothing, symbols)
--                 else (Just (Node { stateRule = newState, term = Just symbol, children = [], parentId = Just parentNode }), symbols)

--         updatedChildren = case childNode of
--             Just c -> (symbol, c) : children parentNode
--             Nothing -> children parentNode

--         updatedNode = parentNode { children = updatedChildren }
--     in
--         createChildNodes grammar updatedNode remainingSymbols

 
-- getTerm :: Node -> Maybe String
-- getTerm node = term node

-- transition :: Node -> String -> Maybe Node
-- transition node term =
--     case find (\child -> isJust (getTerm child) && fromJust (getTerm child) == term) (children node) of
--         Just matchingChild -> Just matchingChild
--         Nothing -> Nothing
 

-- getChildRules :: Node -> [RulesDict]
-- getChildRules node = map stateRule (children node)

-- getUniqueChildren :: Node -> [Node]
-- getUniqueChildren node =
--     let descendants = getaAllChildren node
--     in nub descendants

-- getaAllChildren :: Node -> [Node]
-- getaAllChildren node = 
--     let childrenList = children node
--         descendantsList = concatMap getaAllChildren childrenList
--     in node : descendantsList




-- getLR0parser :: String -> LR0Parser
-- getLR0parser grammar = initLR0Parser gr

-- gr = initGrammar "S -> abS\nS -> c"
-- test = initGraphStack gr "S'"
-- t2 = getLR0parser
-- testChild = getUniqueChildren test 

--ghci :set -package mtl





 