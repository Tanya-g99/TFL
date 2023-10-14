module FSMtoReg where
import FSM
import Normalization
 
solve :: [[RegExp]] -> [RegExp] -> [RegExp]
solve [] [] = []
solve ((a11:a1J) : rows) (a1':aI') = simp x1 : xI where
-- a11 - угловой элемент, а a1J = [a12,...,a1n] - остальная часть 1-й строки
-- rows - это остальные строки [[a21,...,a2n], ..., [an1,...,ann]]
-- a1' - первая константа; aI' - остальные константы [a2',...,an']
  -- первый столбец [a21, ..., an1]
  aI1 = map head rows
  -- подматрица [[a22,...,a2n], ..., [an2,...,ann]]
  aIJ = map tail rows
  -- [[a22_bar,...,a2n_bar], ..., [an2_bar,...,ann_bar]]  вычисляется с помощью f' 
  aIJ_bar = zipWith f aI1 aIJ            -- циклы для i = 2 .. n
  f ai1 aiJ = zipWith (f' ai1) a1J aiJ  -- циклы для j = 2 .. n
  f' ai1 a1j aij = Union [ Cat [ai1, Star a11, a1j], aij]

  -- [a2'_bar,..., an '_bar] вычисляется с помощью func
  aI'_bar = zipWith func aI1 aI'
  func ai1 ai' = Union [ Cat [ai1, Star a11, ai'], ai']
    
  -- решение системы размера n-1
  xI = solve aIJ_bar aI'_bar

  -- вычислить x1 из xI через
  x1 =
    Cat [Star a11, 
    Union (zipWith (\aij xi -> Cat [aij, xi]) a1J xI ++ [a1'])]


toLTS :: FSM Int -> ([[RegExp]], [RegExp])  
toLTS (qs,s,fs,transition,alphabet) = (lIJ, lI') where
   
  lIJ = [[simp (coef i j) | j <- qs] | i <- qs]
  coef i j = Union [Let a | a <- alphabet, (lookupMy i j a transition)] where
    lookupMy :: Int -> Int -> Char -> [(Int, Int, Char)] -> Bool
    lookupMy _ _ _ [] = False
    lookupMy f s ch (c:cs) | ((f, s, ch) == c) = True
                           | otherwise = lookupMy f s ch cs    

  lI' = [ check qi | qi <- qs] where
    check q = if elem q fs then Eps else Zero


convFSMtoStr :: FSM Int -> String
convFSMtoStr m@(qs,s,fs,trans,alf) = regExpToString $ simp $ solution !! s where
  (matrix, consts) = toLTS m
  solution = solve matrix consts


t = convFSMtoStr $ makeIntFSM "(c|a)*"


