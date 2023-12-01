module Grammar where
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Char
import Data.List.Split
import Data.List

epsilon :: Char
epsilon = 'ϵ'

separator :: String
separator = "\n"

alternative :: String
alternative = "|"

partsSeparater :: String
partsSeparater = "->"

data Grammar = Grammar {
    rules :: [GrammarRule], 
    nterms :: (Set String) 
    -- alphabet :: (Set [Char]) 
    } deriving (Eq, Ord, Show)

data GrammarRule = GrammarRule {
    nterm :: String,
    product :: [String]
} deriving (Eq, Ord, Show)

isNtermChar :: Char -> Bool
isNtermChar x = isUpper x || x == '_'

isNterm :: String -> Bool
isNterm "" = False
isNterm str = all isNtermChar str

getTerms :: GrammarRule -> [String]
getTerms rule = filter (not . isNterm) (Grammar.product rule)

getNterms :: GrammarRule -> [String]
getNterms = filter isNterm . Grammar.product

initGrammar :: [GrammarRule] -> (Set String) -> Grammar
initGrammar rules nterms = Grammar rules nterms

stripLeadingWhitespace :: String -> String
stripLeadingWhitespace = unlines . map (dropWhile isSpace) . lines

initGrammarFromString :: String -> Grammar
initGrammarFromString string = let
    rules = concat (map parseRuleString (splitOn separator (trim string)))
    in Grammar rules (Set.fromList (concat (map getNterms rules)))

getInitNterm :: Grammar -> String
getInitNterm = nterm . head . rules

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parseRuleString :: String -> [GrammarRule]
parseRuleString string = let 
    [left, right] = (splitOn partsSeparater (trim string))
    nterm = trim left
    in map (GrammarRule nterm) (map prepareProduct (splitOn alternative right))

splitTermAndNterm :: String -> [String]
splitTermAndNterm "" = []
splitTermAndNterm string = let
    splitTermAndNterm' :: String -> String -> [String] -> [String]
    splitTermAndNterm' "" nterm res
        | null nterm = res
        | otherwise = res ++ [nterm]
    splitTermAndNterm' (x:str) nterm res
        | isNtermChar x = splitTermAndNterm' str (nterm ++ [x]) res
        | otherwise = let
            newRes = 
                if null nterm 
                then res
                else res ++ [nterm]
            newTerm = 
                if x == epsilon
                then ""
                else [x]
            in splitTermAndNterm' str "" (newRes ++ [newTerm])
    in splitTermAndNterm' string "" []

prepareProduct :: String -> [String]
prepareProduct product = concat (map splitTermAndNterm ((splitOn " " . trim) product))
