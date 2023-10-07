module Normalization where
import Data.List
 

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

data RegExp = Zero | Eps | Let Char |
               Union [RegExp] | Cat [RegExp] | Shuf [RegExp] | Star RegExp
  deriving (Eq, Ord, Show)

-- ∅ ϵ ·
toRegExp :: String -> RegExp
toRegExp w = parse w [] where
  parse [] [r] = r
  parse ('∅':xs) rs = parse xs (Zero:rs)
  parse ('ϵ':xs) rs = parse xs (Eps:rs)
  parse ('|':xs) (r2:r1:rs) = parse xs (Union [r1,r2]:rs)
  parse ('#':xs) (r2:r1:rs) = parse xs (Shuf [r1,r2]:rs)
  parse ('·':xs) (r2:r1:rs) = parse xs (Cat [r1,r2]:rs)
  parse ('*':xs) (r:rs) = parse xs (Star r:rs)
  parse (x:xs) rs = parse xs (Let x:rs)
  
 
 
cat :: [RegExp] -> RegExp
cat [] = Eps
cat [r] = r
cat rs = Cat rs
 

star :: RegExp -> RegExp
star Zero = Eps
star Eps = Eps
star (Star r) = star r
star r = Star r

--regular expression simplifier
simp :: RegExp -> RegExp
simp Zero = Zero
simp Eps = Eps
simp (Let c) = Let c
simp (Union rs) = union' $ flat_uni $ map simp rs
simp (Shuf rs) = union' $ flat_shuf $ map simp rs
simp (Cat rs) = union'' $ flat_cat $ map simp rs
simp (Star r) = star $ simp r

norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
  rad :: Eq a => [a] -> [a]   
  rad [] = []
  rad [x] = [x]
  rad (x:ys@(y:zs)) | x == y = rad ys
                    | otherwise = x : rad ys

union' :: [RegExp] -> RegExp
union' rs =  case norm rs of
  []  ->  Zero
  [r] -> r
  rs  -> Union rs

union'' :: [RegExp] -> RegExp
union'' rs =  case norm rs of
  []  ->  Zero
  [r] -> r
  rs  -> Shuf rs
 

flat_uni :: [RegExp] -> [RegExp]
flat_uni [] = []
flat_uni (Zero:rs) = flat_uni rs
flat_uni (Union rs':rs) = rs' ++ flat_uni rs
flat_uni (r:rs) = r : flat_uni rs

flat_shuf :: [RegExp] -> [RegExp]
flat_shuf [] = []
flat_shuf (Zero:rs) = flat_shuf rs
flat_shuf (Shuf rs':rs) = rs' ++ flat_shuf rs
flat_shuf (r:rs) = r : flat_shuf rs
 
 
 
flat_cat :: [RegExp] -> [RegExp]
flat_cat rs = fc [] rs where
-- fc (аргументы уже обработаны) (аргументы будут обрабатываться)
  fc :: [RegExp] -> [RegExp] -> [RegExp]
  fc pr [] = [cat $ reverse pr]
  fc pr (Zero:rs) = []
  fc pr (Eps:rs) = fc pr rs
  fc pr (Cat rs':rs) = fc (reverse rs' ++ pr) rs
  fc pr (Union rs':rs) = concat $ map (fc pr . (:rs)) rs'
  fc pr (r:rs) = fc (r:pr) rs

getNorm :: String -> RegExp
getNorm regex = simp $ toRegExp $ toPostfix $ regex


test1 = toPostfix "ba#c|"
test = toRegExp test1
test_d = simp $ test
 