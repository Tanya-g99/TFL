module Normalization where
import Data.List

data RegExp = Zero | Eps | Let Char |
               Union [RegExp] | Cat [RegExp] | Shuf [RegExp] | Star RegExp
  deriving (Eq, Ord, Show)

-- ∅ ϵ ·
toRegExp :: String -> RegExp
toRegExp w = parse w [] where
  parse :: String -> [RegExp] -> RegExp
  parse [] [r] = r
  parse ('∅':xs) rs = parse xs (Zero:rs)
  parse ('ϵ':xs) rs = parse xs (Eps:rs)
  parse ('|':xs) (r2:r1:rs) = parse xs (Union [r1,r2]:rs)
  parse ('#':xs) (r2:r1:rs) = parse xs (Shuf [r1,r2]:rs)
  parse ('·':xs) (r2:r1:rs) = parse xs (Cat [r1,r2]:rs)
  parse ('*':xs) (r:rs) = parse xs (Star r:rs)
  parse (x:xs) rs = parse xs (Let x:rs)

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

removeStar :: String -> String
removeStar lst = let
    removeStar' [op] res = reverse (op:res)
    removeStar' (op1:op2:lst) res
      | op1 == '*' && op2 == '*' = removeStar' (op2:lst) res
      | otherwise = removeStar' (op2:lst) (op1:res)
  in removeStar' lst ""

toPostfix :: String -> String
toPostfix regex = removeStar lastStep
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
  


cat :: [RegExp] -> RegExp
cat [] = Eps
cat [r] = r
cat rs = Cat rs

shuf :: [RegExp] -> RegExp
shuf [] = Eps
shuf [r] = r
shuf rs = Shuf rs

uni :: [RegExp] -> RegExp
uni [] = Eps
uni [r] = r
uni rs = Union rs

star :: RegExp -> RegExp
star Zero = Eps
star Eps = Eps
star (Star r) = star r
star r = Star r
 

simpStar :: RegExp -> RegExp
simpStar reg = simp [reg] False where
  simp :: [RegExp] -> Bool -> RegExp 
  simp [Cat rs] b = cat[(simp [ri] False) | ri <- rs]
  simp [Shuf rs] b = shuf[(simp [ri] False) | ri <- rs]
  simp [Union rs] b = if b == True then uni[(simp [ri] True) | ri <- rs] else uni[(simp [ri] False) | ri <- rs]
  simp [Star x] b = if b == True then simp [x] True else star(simp [x] True)
  simp [Let c] b = Let c
  simp [Eps] b = Eps

union'' :: [RegExp] -> RegExp
union'' [rs] = rs
 
  
--regular expression simplifier
simp :: RegExp -> RegExp
simp Zero = Zero
simp Eps = Eps
simp (Let c) = Let c
simp (Union rs) = union' $ flat_uni $ map simp rs
simp (Shuf rs) = union' $ flat_shuf $ map simp rs
simp (Cat rs) = union' $ flat_cat $ map simp rs
simp (Star r) = star $ simp r


union' :: [RegExp] -> RegExp
union' rs =  case norm rs of
  []  ->  Zero
  [r] -> r
  rs  -> Union rs

norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
  rad :: Eq a => [a] -> [a]   
  rad [] = []
  rad [x] = [x]
  rad (x:ys@(y:zs)) | x == y = rad ys
                    | otherwise = x : rad ys


flat_uni :: [RegExp] -> [RegExp]
flat_uni [] = []
flat_uni (Zero:rs) = flat_uni rs
flat_uni (Union rs':rs) = rs' ++ flat_uni rs
flat_uni (r:rs) = r : flat_uni rs


flat_shuf :: [RegExp] -> [RegExp]
flat_shuf rs = fc [] rs where
  fc :: [RegExp] -> [RegExp] -> [RegExp]
  fc pr [] = [shuf $ reverse pr]
  fc pr (Zero:rs) = []
  fc pr (Eps:rs) = fc pr rs
  fc pr (Shuf rs':rs) = fc (reverse rs' ++ pr) rs
  fc pr (Union rs':rs) = concat $ map (fc pr . (:rs)) rs'
  fc pr (r:rs) = fc (r:pr) rs
 
  
flat_cat :: [RegExp] -> [RegExp]
flat_cat rs = fc [] rs where
  fc :: [RegExp] -> [RegExp] -> [RegExp]
  fc pr [] = [cat $ reverse pr]
  fc pr (Zero:rs) = []
  fc pr (Eps:rs) = fc pr rs
  fc pr (Cat rs':rs) = fc (reverse rs' ++ pr) rs
  fc pr (Union rs':rs) = concat $ map (fc pr . (:rs)) rs'
  fc pr (r:rs) = fc (r:pr) rs

getNorm :: Char -> String -> RegExp
getNorm a regex = simp $ deriv a $ toRegExp $ toPostfix $ insertDot regex
 
nullable :: RegExp -> Bool
nullable Zero = False 
nullable Eps = True
nullable (Let c) = False
nullable (Union rs) = or [nullable r | r <- rs]
nullable (Shuf rs) = or [nullable r | r <- rs]
nullable (Cat rs) = and [nullable r |  r <- rs]
nullable (Star r) = True 

dCat :: Char -> [RegExp] -> [RegExp]
dCat a [] = []
dCat a (r:rs) = (Cat((deriv a r) :rs) ) : (if nullable r then dCat a rs else [])

dShuf :: Char -> [RegExp] -> [RegExp]
dShuf a [] = []
dShuf a (r:rs) = (Shuf((deriv a r) :rs)) : (Shuf(r : (map (\q -> deriv a q) rs))) : []

deriv :: Char -> RegExp -> RegExp
deriv _ Zero = Zero
deriv _ Eps = Zero
deriv a (Let c)
 | a == c = Eps
 | otherwise = Zero
deriv a (Union rs) = Union (map (\r -> deriv a r) rs)
deriv a (Cat rs) = Union (dCat a rs)
deriv a (Shuf rs) = Union(dShuf a rs)
deriv a (Star r) = Cat[deriv a r, (Star r)]
