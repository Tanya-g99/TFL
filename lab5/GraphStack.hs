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


findSingleNode :: Map Node [(String, Node)] -> Set Node
findSingleNode transMatrix =
  let singleElementNodes = Map.keys $ Map.filter ((== 1) . length) transMatrix
  in Set.fromList singleElementNodes


isValid :: Set Node -> (String, Node) -> Bool
isValid allKeys (_, node) = Set.member node allKeys
--
checkAndRemoveInvalidPairs :: Map Node [(String, Node)] -> Map Node [(String, Node)]
checkAndRemoveInvalidPairs transMatrix =
  let allKeys = Set.fromList $ Map.keys transMatrix
      updatedPairs = Map.map (Data.List.filter (isValid allKeys)) transMatrix
  in updatedPairs



popTop :: Node ->  GraphStack -> GraphStack
popTop deleteNode (GraphStack topNodes transMatrix) =
  let singleElementNodes = findSingleNode transMatrix
      updatedTransMatrix = removeKeyInTransMatrix deleteNode transMatrix
      updatedMatrixWithNodesRemoved = removeChildrenInMatrix deleteNode singleElementNodes updatedTransMatrix

      resultMatrix = checkAndRemoveInvalidPairs updatedMatrixWithNodesRemoved
      updatedTopNodes =  Data.List.filter (/= deleteNode) topNodes
  in GraphStack updatedTopNodes resultMatrix

removeKeyInTransMatrix :: Node -> Map Node [(String, Node)] -> Map Node [(String, Node)]
removeKeyInTransMatrix key transMatrix = Map.delete key transMatrix
 
 
removeChildrenInMatrix :: Node -> Set Node -> Map Node [(String, Node)] -> Map Node [(String, Node)]
removeChildrenInMatrix key singleElementNodes transMatrix =
  let keysToRemove = findChildrenToRemove key singleElementNodes transMatrix
  in Data.List.foldr Map.delete transMatrix keysToRemove

findChildrenToRemove :: Node -> Set Node -> Map Node [(String, Node)] -> [Node]
findChildrenToRemove key singleElementNodes transMatrix =
  let parents = Map.keysSet $ Map.filter (any (\(_, v) -> v == key)) transMatrix
      removableParents = Set.toList $ Set.intersection parents singleElementNodes
      recursiveChildren = concatMap (\node -> findChildrenToRemove node singleElementNodes transMatrix) removableParents
  in key : recursiveChildren



---TEST

graph = initGraphStack
initialNode = Node { stateId = 0, wordState = 0 }
node4 = Node { stateId = 4, wordState = 4 }
node2 = Node { stateId = 1, wordState = 1 }
node3 = Node { stateId = 3, wordState = 3 }
node5 = Node { stateId = 5, wordState = 5 }

--0 -- 2 -- 3 -- 5
-- 0 -- 4


test = push node2 "a" initialNode graph
test2 = push node3 "c" node2 test
test22 = push node4 "n" initialNode test2
test222 = push node5 "f" node3 test22

test3 = getParents node3 "c" test2
test4 = findSingleNode (transMatrix test2)
test5 = popTop node5 test222





 