module GraphStack where
import Grammar
import LR0 
import Data.Map  
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude  hiding (product)
import Data.List  
import Data.Maybe (isNothing)
import Data.Function (on)

data GraphStack = GraphStack {
    listTopNodes :: [Node],
    transMatrix :: Map Node [(String, Node)]
}  
data Node = Node {
    stateId :: Int,
    wordState :: Int
} deriving (Eq, Ord)

instance Show Node where
  show (Node stateId wordState) = "{ " ++ show stateId ++ ",  " ++ show wordState ++ "}" ++ "\n"

instance Show GraphStack where
  show (GraphStack topNodes transMatrix) = "GraphStack {listTopNodes = " ++ 
                                            show topNodes ++
                                             "\n" ++
                                            "transMatrix = " ++ 
                                             "\n" ++
                                            show transMatrix ++ "}"


initGraphStack :: GraphStack
initGraphStack =
  let initialNode = Node { stateId = 0, wordState = 0 }
      initialMatrix = Map.fromList [(initialNode, [])]
  in GraphStack { listTopNodes = [initialNode], transMatrix = initialMatrix }

getParents :: Node -> String -> GraphStack -> [Node]
getParents child string (GraphStack _ transMatrix) =
  let matchingNodes = [node | (node, transitions) <- Data.Map.toList transMatrix, (transition, targetNode) <- transitions, transition == string, targetNode == child]
  in matchingNodes


push :: Node -> String -> Node -> GraphStack -> GraphStack
push newNode string parentNode (GraphStack topNodes transMatrix) =
  let updatedMatrix = Map.insertWith (++) parentNode [(string, newNode)] transMatrix

      newNodeExists = isNothing (Map.lookup newNode transMatrix)

      updatedMatrix' = if (newNodeExists) then Map.insert newNode [] updatedMatrix  else updatedMatrix
        
      updatedTopNodes = if any (\n -> stateId n == stateId parentNode) topNodes
                        then Data.List.map (\n -> if stateId n == stateId parentNode then newNode else n) topNodes
                        else newNode : topNodes
  in GraphStack updatedTopNodes updatedMatrix'

-- popTop :: Node -> GraphStack -> GraphStack
-- popTop node (GraphStack topNodes transMatrix) =
--   let allNodes = Map.keysSet transMatrix
--       nodesToDelete = findNodesToDelete node allNodes transMatrix
--       updatedTransMatrix = Data.List.foldr Map.delete transMatrix (Set.toList nodesToDelete)
--       updatedTopNodes = Data.List.filter (\n -> n /= node) topNodes
--   in GraphStack updatedTopNodes updatedTransMatrix

-- findNodesToDelete :: Node -> Set Node -> Map Node [(String, Node)] -> Set Node
-- findNodesToDelete node allNodes transMatrix =
--   if Map.member node transMatrix && all (\(_, v) -> not (Set.member v allNodes)) (snd <$> (transMatrix Map.! node))
--   then let parents = Map.keysSet (Map.filter (any (\(_, v) -> v == node)) transMatrix)
--            toDelete = Set.difference parents allNodes
--        in Set.unions (toDelete : [findNodesToDelete n allNodes transMatrix | n <- Set.toList toDelete])
--   else Set.empty

---TEST

graph = initGraphStack
initialNode = Node { stateId = 0, wordState = 0 }
secondNode = Node { stateId = 1, wordState = 1 }
thirdNode = Node { stateId = 2, wordState = 2 }
test = push secondNode "a" initialNode graph
test2 = push thirdNode "c" secondNode test
test3 = getParents thirdNode "c" test2




-- getParents :: Node -> String -> [Node]
-- getParents curNode term = let
--     checkNodes :: [Maybe Node] -> [Node] -> [Node]
--     checkNodes [] nodes = nodes
--     checkNodes (mn:mnodes) nodes = 
--         case mn of
--             Just node -> checkNodes mnodes 
--                 (if ((term, curNode) `elem` (children node))
--                     then (node:nodes)
--                     else nodes)
--     in checkNodes (parentNode curNode) []

-- push :: Node -> String -> GraphStack -> GraphStack
-- push newNode string (GraphStack topNodes stackNode) =
--     case findSameNode newNode stackNode of
--         Just existingNode -> 
--             let updatedStackNode' = addToExistingNode newNode string existingNode stackNode
--                 updatedStackNode = findRoot updatedStackNode'  -- Возвращаем стек в корне
--                 updatedTopNodes = replaceParentNode newNode (head (parentNode newNode)) topNodes
--             in GraphStack updatedTopNodes updatedStackNode
--         Nothing ->
--             let
--                 parentNode' = fromJust $ head (parentNode newNode)
--                 updatedParentNode = fromJust (findSameNode parentNode' stackNode)  -- Найдем узел-родитель в стеке

--                 updatedChildren = (string, newNode) : children updatedParentNode  -- Обновляем список детей узла-родителя
--                 updatedParentNode' = updatedParentNode {children = updatedChildren}

--                 updatedStackNode = findRoot updatedParentNode'  -- Найдем корневой узел для новой ноды

--                 updatedTopNodes = replaceParentNode newNode (Just updatedStackNode) topNodes  -- Обновляем список верхних узлов
--             in GraphStack updatedTopNodes updatedParentNode'

-- findRoot :: Node -> Node
-- findRoot curNode
--     | stateId curNode == 0 = curNode  -- Если текущий узел - корень, возвращаем его
--     | otherwise = case parentNode curNode of
--                     [] -> curNode  -- Если у узла нет родителей, то текущий узел будет корнем
--                     parents -> findRoot $ fromJust $ head $ parents


-- -- findRoot :: Node -> Node
-- -- findRoot curNode
-- --     | stateId curNode == 0 = curNode  -- Если текущий узел - корень, возвращаем его
-- --     | otherwise = case parentNode curNode of
-- --                     [] -> curNode  -- Если у узла нет родителей, то текущий узел будет корнем
-- --                     parents -> case potentialRoots of
-- --                                     [] -> findRoot (head $ mapMaybe id parents)  -- Если нет потенциальных корневых узлов, продолжаем поиск среди родителей текущего узла
-- --                                     (root:_) -> findRoot (fromJust root)  -- Иначе выбираем первый найденный потенциальный корневой узел и продолжаем поиск вверх
-- --                         where potentialRoots = filter (\x -> stateId (fromJust x) == 0) parents

-- findSameNode :: Node -> Node -> Maybe Node
-- findSameNode newNode currentNode =
--     if newNode == currentNode
--         then Just currentNode
--         else case find (\(_, node) -> findSameNode newNode node /= Nothing) (children currentNode) of
--             Just (_, foundNode) -> findSameNode newNode foundNode
--             Nothing -> Nothing

-- addToExistingNode :: Node -> String -> Node -> Node -> Node
-- addToExistingNode newNode string existingNode currentNode =
--     if existingNode == currentNode
--         then existingNode {parentNode = ((parentNode newNode) ++ parentNode existingNode)}
--         else currentNode {children = map (\(key, node) -> if node == existingNode 
--             then (key, addToExistingNode newNode string existingNode node) else (key, node)) (children currentNode)}
 

-- replaceParentNode :: Node ->  Maybe Node -> [Node] -> [Node]
-- -- replaceParentNode _ _ [] = []
-- replaceParentNode newNode maybeParentNode topNodes = 
--     case findNodeIndex parentNode topNodes of
--         Just index -> let (front, _ : rest) = splitAt index topNodes
--                       in front ++ [newNode] ++ rest
--         Nothing -> topNodes ++ [newNode]
--   where parentNode = fromJust maybeParentNode


-- initGraphStack :: GraphStack
-- initGraphStack = GraphStack [emptyNode] emptyNode
--   where emptyNode = Node 0 0 [] [Nothing]


-- findNodeIndex :: Node -> [Node] -> Maybe Int
-- findNodeIndex _ [] = Nothing
-- findNodeIndex node nodeList = findIndexHelper node nodeList 0
--   where
--     findIndexHelper :: Node -> [Node] -> Int -> Maybe Int
--     findIndexHelper _ [] _ = Nothing
--     findIndexHelper targetNode (x:xs) currentIndex
--       | targetNode == x = Just currentIndex
--       | otherwise = findIndexHelper targetNode xs (currentIndex + 1)

-- graph = GraphStack { listTopNodes = [], stackNodes = node1 }
-- node1 = Node 0 0  [] [Nothing]
-- node2 = Node 2 20 [] [Just node1]
-- node3 = Node 3 30 [] [Just node2]
-- test2 = push node2 "a" graph 
-- test3 = push node3 "bab" test2
-- node1 = Node 0 0  [("a", node2), ("b", node3)] [Nothing]
-- node2 = Node 2 20 [] [Just node1]
-- node3 = Node 3 30 [] [Just node1]
-- node4 = Node 4 40 [] [Just node2]
-- node5 = Node 5 50 [] [Just node3]

-- test = findRoot node5



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





 