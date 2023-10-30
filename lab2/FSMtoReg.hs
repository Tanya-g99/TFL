module FSMtoReg where
import FSM
import Normalization
 
solve :: [[RegExp]] -> [RegExp] -> [RegExp]
solve [] [] = []
solve ((a11:a1J) : rows) (a1':aI') 
 | length (a1':aI') > 1 = solve aIJ_bar b_bar'
 | otherwise = b_bar
  where
    --первая строка
     fRow = a11:a1J
     --измененная первая строка A[0,j] := star(A[0,0]) . A[0,j]
     changefRow = map (\r -> Cat[Star(a11), r]) fRow
    --объединяем измененную первую строку с остальными 
     aIJ = changefRow : rows 
    -- ind - размерность квадратной матрицы
     ind = (length (head aIJ)) - 1 
     --матрица где перемножены A[i,0] . A[0,j]
     aIJ_bar' = [[Cat [(aIJ!!i!!0), (aIJ!!0!!j)] | j <- [0..ind]] | i <- [0..ind] ]
   --матрица где сложены A[i,j] += A[i,n] . A[n,j]
     aIJ_bar'' = [[Union[aIJ!!i!!j, aIJ_bar'!!i!!j] | j <- [0..ind]] | i <- [0..ind] ]
  -- получаем  следующую матрицу, которую нужно решить, убирая 1 столбец и строку
     aIJ_bar = (map tail (tail aIJ_bar''))
 -- вычисляем первый элемент B[0] := star(A[0,0]) . B[0]
     b1 = Cat[Star(a11), a1']
 -- объединаем измененный первый элемент к остальным
     b' = b1:aI'
 -- вычисляем  B[i] += A[i,0] . B[0]  --это будет решением когда будет последний угловой элемент
     b_bar = [Union[Cat[aIJ!!i!!0, b1], b'!!i] | i <- [0..ind]]
-- убираем первый элемент для следующей итерации
     b_bar' = (tail b_bar)

toLTS :: FSM Int -> ([[RegExp]], [RegExp])  
toLTS (qs,s,fs,transition,alphabet) = (lIJ, lI') where
   
  lIJ = [[simp (coef i j) | j <- qs] | i <- qs]
  coef i j = Union [Let a | a <- alphabet, (lookupMy i j a transition)] where
    lookupMy :: Int -> Int -> Char -> [Transition] -> Bool
    lookupMy _ _ _ [] = False
    lookupMy f s ch (c:cs) | ((f, s, ch) == c) = True
                           | otherwise = lookupMy f s ch cs    

  lI' = [ check qi | qi <- qs] where
    check q = if elem q fs then Eps else Zero
 

revL :: [a] -> [a]
revL [] = []
revL (x:xs) = revL xs ++ [x]

revLL :: [[a]] -> [[a]]
revLL []     =  []
revLL (x:xs) = (revLL xs) ++ [reverse x] 
