module Main where
import Text.Printf

data Node = Node { oper :: Char, l :: Maybe Node, r :: Maybe Node }

priority :: Char -> Int
priority '*' = 4
priority '·' = 3
priority '#' = 2
priority '|' = 1
priority _ = -1

leftAssoc :: Char -> Bool
leftAssoc '*' = False
leftAssoc _ = True

isOp :: Char -> Bool
isOp token = token `elem` "*·#|"

insertDot :: String -> String
insertDot [] = "ϵ"
insertDot regex = let 
        insertDot' :: String -> Char -> String -> String
        insertDot' res prevChar "" = res
        insertDot' res prevChar regex
            | ((head regex) `notElem` "#|*)") 
            && (prevChar `notElem` "(#|") = 
                insertDot' ((head regex) : ('·' : res)) 
                            (head regex) 
                            (tail regex)
            | otherwise = 
                insertDot' ((head regex) : res) 
                            (head regex) 
                            (tail regex)
    in ((head regex) : (reverse (insertDot' "" (head regex) (tail regex))))

toPostfix :: String -> String
toPostfix regex = lastStep
  where
    lastStep = 
        (\(output, stack, _) -> reverse((reverse stack) <> output))
        $ last toPostfix'
    toPostfix' = scanl parse ("", "", ' ') regex
    parse (output, stack, _) token
        | isOp token =
            (reverse (takeWhile testOp stack) <> output,
            (token :) (dropWhile testOp stack),
            token)
        | token == '(' = 
            (output, 
            '(' : stack, 
            token)
        | token == ')' =
            (reverse (takeWhile (/= '(') stack) <> output,
            tail $ dropWhile (/= '(') stack,
            token)
        | otherwise = 
            (token : output, 
            stack, 
            token)
        where
            testOp x =
                isOp x
                && (leftAssoc token 
                    && priority token == priority x
                    || priority token < priority x)

postfixToTree :: String -> Maybe Node
postfixToTree postfix = postfixToTree' postfix []
  where
    postfixToTree' :: String -> [Maybe Node] -> Maybe Node
    postfixToTree' "" stack = last stack
    postfixToTree' (c:cs) stack
      | c `elem` "|·#" = 
        let (r:l:rest) = stack in
            postfixToTree' cs ((Just (Node c l r)) : rest)
      | c == '*' =
        let (l:rest) = stack in
            postfixToTree' cs ((Just (Node c l Nothing)) : rest)
      | otherwise = postfixToTree' cs ((Just (Node c Nothing Nothing)) : stack)
        
inorder :: Maybe Node -> String
inorder Nothing = ""
inorder (Just root) = 
    let out = inorder (l root) ++ oper root : inorder (r root) in
      if ((oper root) `elem` "|·*#")
        then '(' : out ++ ")"
        else out

nullable :: Maybe Node -> Bool
nullable node = 
  case node of 
    Nothing -> False
    Just node -> 
      case oper node of
        'ϵ' -> True
        '*' -> True
        '·' -> nullable (l node) && nullable (r node)
        '|' -> nullable (l node) || nullable (r node)
        _ -> False

deriv :: Char -> Maybe Node -> Maybe Node
deriv c root = case root of 
  Nothing -> Nothing
  Just node -> case oper node of
    '∅' -> Just node
    '|' -> (Just (Node '|' 
                        (deriv c (l node)) 
                        (deriv c (r node))))
    '·' -> if nullable (l node) 
            then Just (Node '|'
                            (Just (Node '·' 
                                        (deriv c (l node))
                                        (r node)))
                            (deriv c (r node)))
            else Just (Node '·' 
                            (deriv c (l node))
                            (r node))
    '#' -> (Just (Node '|'
                        (Just (Node '#' 
                                    (deriv c (l node))
                                    (r node)))
                        (Just (Node '#'
                                    (l node)
                                    (deriv c (r node))))
                        ))
    '*' -> (Just (Node '·'
                        (deriv c (l node))
                        (Just node)))
    _ | oper node == c -> (Just (Node 'ϵ' Nothing Nothing))
      | otherwise -> (Just (Node '∅' Nothing Nothing))

convert :: String -> Char -> String
convert regex derChar = inorder $ deriv derChar $ postfixToTree . toPostfix . insertDot $ regex

main :: IO ()
main = do
  regex <- getLine
  derChar <- getChar
  printf "\n%s\n" $ convert regex derChar
