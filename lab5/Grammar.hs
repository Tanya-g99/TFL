module Grammar where
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char
import Data.List.Split
import Data.List

epsilon :: Char
epsilon = 'ϵ'

space :: Char
space = '_'

newline :: Char
newline = '$'

separator :: String
separator = "\n"

alternative :: String
alternative = "|"

partsSeparator :: String
partsSeparator = "->"

data Grammar = Grammar {
    rules :: [GrammarRule], 
    nterminals :: (Set String),
    alphabet :: (Set String)
} deriving (Eq, Ord, Show)

data GrammarRule = GrammarRule {
    dotIndex :: Int,
    nterminal :: String,
    product :: [String]
} deriving (Eq, Ord)

instance Show GrammarRule where
    show (GrammarRule dotIndex nterminal productions) =
        let 
            replaceSpaceNewline :: String -> String
            replaceSpaceNewline term
                | term == " " = [space]
                | term == "" = [epsilon]
                | term == "\n" = [newline]
                | otherwise = term
            
            product = map replaceSpaceNewline productions
            beforeDot = unwords $ take dotIndex $ product
            afterDot = unwords $ drop dotIndex $ product
            showProduct = 
                if (dotIndex >= 0)
                    then beforeDot ++ "." ++ afterDot
                    else unwords product
        in nterminal ++ " -> " ++ showProduct

type RulesDict = Map String (Set GrammarRule)

genRulesDict ::  Grammar -> RulesDict
genRulesDict grammar = Map.fromListWith Set.union $ concatMap (\rule -> [(nterminal rule, Set.singleton rule)]) (rules grammar)

makeRulesDict ::  [GrammarRule] -> RulesDict
makeRulesDict rules = Map.fromListWith Set.union $ concatMap (\rule -> [(nterminal rule, Set.singleton rule)]) rules

isNterminalChar :: Char -> Bool
isNterminalChar c = any ($ c) [isUpper, (=='\'')]

isNterminal :: String -> Bool
isNterminal "" = False
isNterminal str = all isNterminalChar str

getterminals :: GrammarRule -> [String]
getterminals = filter (not . isNterminal) . Grammar.product

getNterminals :: GrammarRule -> [String]
getNterminals = filter isNterminal . Grammar.product

stripLeadingWhitespace :: String -> String
stripLeadingWhitespace = unlines . map (dropWhile isSpace) . lines

initGrammar :: String -> Grammar
initGrammar string = let
    rules = concat (map parseRuleString (splitOn separator (trim string)))
    in Grammar rules 
        (Set.fromList (map (nterminal) rules))
        (Set.fromList (concat (map getterminals rules)))

getInitNterminal :: Grammar -> String
getInitNterminal = nterminal . head . rules

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parseRuleString :: String -> [GrammarRule]
parseRuleString string = let 
    [left, right] = (splitOn partsSeparator (trim string))
    nterminal = trim left
    in map (GrammarRule (-1) nterminal) (map prepareProduct (splitOn alternative right))

splitTerminalAndNterminal :: String -> [String]
splitTerminalAndNterminal "" = []
splitTerminalAndNterminal string = let
    makeTerminal :: Char -> String
    makeTerminal char
        | char == epsilon = ""
        | char == space = " "
        | char == newline = "\n"
        | otherwise = [char]
    splitTerminalAndNterminal' :: String -> String -> [String] -> [String]
    splitTerminalAndNterminal' "" nterminal res
        | null nterminal = res
        | otherwise = res ++ [nterminal]
    splitTerminalAndNterminal' (x:str) nterminal res
        | isNterminalChar x = splitTerminalAndNterminal' str (nterminal ++ [x]) res
        | otherwise = let
            newRes = 
                if null nterminal 
                then res
                else res ++ [nterminal]
            newterminal = makeTerminal x
            in splitTerminalAndNterminal' str "" (newRes ++ [newterminal])
    in splitTerminalAndNterminal' string "" []

prepareProduct :: String -> [String]
prepareProduct product = concat (map splitTerminalAndNterminal ((splitOn " " . trim) product))

reverseGrammarRule :: GrammarRule -> GrammarRule
reverseGrammarRule (GrammarRule dotIndex nterminal product) = (GrammarRule dotIndex nterminal (reverse product))

reverseGrammar :: Grammar -> Grammar
reverseGrammar (Grammar rules nterminals alphabet) = (Grammar (map reverseGrammarRule rules) nterminals alphabet)