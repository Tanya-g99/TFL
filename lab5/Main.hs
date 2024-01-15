module Main where
import System.IO
import Data.List
import LR0Parser
import Grammar
import GraphStack

unClose ::  [String] -> String
unClose [rs] = rs
 
concatenateStrings :: [String] -> String
concatenateStrings [] = ""  
concatenateStrings (x:xs) = x ++ concatenateStrings xs  


main = do

  handle <- openFile "grammar.txt" ReadMode
  grammar <- getLinesUntilEmpty handle
 
  hClose handle
 
  let grammarString = reverse $ tail $ reverse $ concatenateStrings grammar
  
  handle <- openFile "word.txt" ReadMode
  word <- getLinesUntilEmpty handle
  hClose handle
  let oneWord = reverse $ tail $ reverse $ concatenateStrings word

-- Заданная вручную константа N
  let stepN = 3

  let (gs, result) = parseString (initLR0Parser (initGrammar grammarString)) oneWord stepN
  print result
  
  print gs
  let dotString = graphStackToDOT gs
  writeFile "graph_stack.dot" dotString
 
  print "The analysis was completed successfully"


getLinesUntilEmpty :: Handle -> IO [String]
getLinesUntilEmpty h = do
  eof <- hIsEOF h
  if eof
    then return []
    else do
      line <- hGetLine h
      if null line
        then return []
        else do
          rest <- getLinesUntilEmpty h
          return (line : "\n" : rest)

 