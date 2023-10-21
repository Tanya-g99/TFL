module Test where
import FSMtoReg
import RandomRegex
import GenerateString
import System.Random
import FSM
import Normalization
import Control.Arrow

convFSMtoStr :: FSM Int -> [[RegExp]] -> [RegExp] -> String
convFSMtoStr (qs,s,fs,trans,alf) matrix consts = regExpToString $ simp $ (solution !! s) where
  solution = solve matrix consts

generate20Strings :: FSM Int -> [[RegExp]] -> Int -> [String]
generate20Strings (states, start, finals, transitions, alphabet) lts i =
  [mutateString ((mkStdGen (-j)), (generateString (mkStdGen j) lts finals )) 
  | j <- [i * 20 + 1 .. i * 20 + 21]]


mainTest :: RandomGen g => g -> Int -> ([Bool], String, String, [String])
mainTest g i =
  let regex = createRegex (mkStdGen(i))
      (states, start, finals, transitions, alphabet) = makeIntFSM regex
      (lts, consts) = toLTS (states, start, finals, transitions, alphabet) 
      akademReg = convFSMtoStr (states, start, finals, transitions, alphabet) lts consts
      words = generate20Strings (states, start, finals, transitions, alphabet) lts i
      result = [if resAkademic == resOriginal then True else False
               | j <- [1 .. 20]
               , let resAkademic = checkWordMatchRegex akademReg (words !! j)
                     resOriginal = checkWordMatchRegex regex (words !! j)] 
  in (result, regex, akademReg, words)

uniqueRandomInts :: (RandomGen g, Random a) => (a, a) -> Int -> g -> ([a], g)
uniqueRandomInts range n = 
            (map fst &&& snd . last) . take n . iterate getNext . randomR range
            where getNext = randomR range . snd

main :: IO ()
main = do
  g <- newStdGen
  let randList = fst $ uniqueRandomInts (0, 10000::Int) 10 g  
  let tests = [mainTest (mkStdGen(i)) i | i <- randList]
  mapM_ printTest  tests 
  where printTest (bool, regex, akademReg, words)    = do
          putStrLn "Группа Тестов" 
          putStrLn $ "Исходная регулярка " ++ regex
          putStrLn $ "Академическая регулярка " ++ akademReg
          mapM_ printWord (zip [1..] bool)
          where printWord (j, b) = if b
                                   then putStrLn $ "Слово " ++ show j ++ " " ++ "OK " ++ "'" ++ (words !! (j)) ++ "'"
                                   else putStrLn $ "Слово " ++ show j ++ " "  ++ " FALL" ++ "'" ++  (words !! (j)) ++ "'"



