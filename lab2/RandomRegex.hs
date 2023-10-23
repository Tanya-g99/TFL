module RandomRegex where
 
import System.IO.Unsafe
import System.Random
import Control.Arrow
--  <init> ::= <regex>
--  <regex> ::= 
--  0 <regex><binary><regex> 
--  1 (<regex>) 
--  2 <regex><unary> 
--  3 <symbol> 
--  4 eps

--  <binary> ::= | | # | eps
--  <unary> ::= *
data RegexConfig = RegexConfig {
  alphabetSize :: Int,
  starLevel :: Int,
  maxLength :: Int,
  len :: Int,
  stateException :: [Int],
  myrand :: [Int]
}
  
generateRegex :: RegexConfig -> String
generateRegex config
  | len config == maxLength config = ""
  | maxLength config - len config < 2 = generateRegex config { stateException = 0 : stateException config, myrand = tail $ myrand config}
  | randomNumber `elem` stateException config || (randomNumber == 2 && starLevel config == 0)  = generateRegex config {myrand = tail $ myrand config}
  | randomNumber == 0 =
    let newConf = config { starLevel = starLevel config - 1}
        binary = generateBinary newConf
        newConf' = newConf { maxLength = maxLength newConf - 1 }
        half1 = generateRegex newConf' {myrand = tail $ myrand newConf' }
        half2 = generateRegex (newConf' { maxLength = maxLength newConf' + 1, myrand =  tail $ reverse $ tail $ myrand newConf'})
    in half1 ++ binary ++ half2
  | randomNumber == 1 =
    let newConf = config { starLevel = starLevel config - 1, stateException = 1 : stateException config, myrand = tail $ myrand config }
        reg = generateRegex newConf
    in "(" ++ reg ++ ")"
  | randomNumber == 2 =
    let newConf = config { starLevel = starLevel config - 1, stateException = [4], myrand = tail $ myrand config}
        reg = generateRegex newConf
        unary = "*"
    in reg ++ unary
  | randomNumber == 3 = generateSymbol config
  | randomNumber == 4 = "ϵ"
  | otherwise = error "out of range"
   where
    randomNumber :: Int  
    randomNumber = head $ myrand config
    -- (randomNumber, _)  =  randomR (0, 4) (myrand config) 

generateBinary :: RegexConfig -> String
generateBinary config
  | len config == maxLength config = ""
  | randomNumber == 0 = "|"
  | randomNumber == 1 = "#"
  | randomNumber == 2 = ""
  | otherwise = error "out of range"
  where
    randomNumber :: Int
    randomNumber =  unsafePerformIO (randomRIO (0, 2))
 
generateSymbol :: RegexConfig -> String
generateSymbol config = [toEnum (97 + randomNumber )]
  where 
    randomNumber:: Int
    randomNumber = unsafePerformIO (randomRIO (0, alphabetSize config - 1))
 
 
createRegex :: RandomGen g => g -> String
createRegex g = generateRegex (RegexConfig alphabetSize starLevel maxLength 0 [] randArray) where
    alphabetSize = 3
    starLevel = 2
    maxLength = 10
    -- stateException = [4]
    randArray = fst $ uniqueRandomInts (0, 4::Int) 1000 g where
        uniqueRandomInts :: (RandomGen g, Random a) => (a, a) -> Int -> g -> ([a], g)
        uniqueRandomInts range n = 
            (map fst &&& snd . last) . take n . iterate getNext . randomR range
            where getNext = randomR range . snd
