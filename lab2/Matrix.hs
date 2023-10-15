module Matrix where
import Data.List

type Matrix = ([[Int]], Int, Int)

makeEmptyMatrix :: Int -> Int -> Matrix
makeEmptyMatrix rows cols 
    | (rows > 0) && (cols > 0) = (replicate rows (replicate cols 0), rows, cols)
    | otherwise = error "Matrix cols and rows must be > 0"

makeIdentityMatrix :: Int -> Int -> Matrix
makeIdentityMatrix rows cols 
    | (rows > 0) && (cols > 0) = let
        onesOnMainDiagonal i j = 
            if (i == j)
                then 1
                else 0
        in  ([[ onesOnMainDiagonal i j | j <- [1..cols]] | i <- [1..rows]], rows, cols)
    | otherwise = error "Matrix cols and rows must be > 0"

addMatrix :: Matrix -> Matrix -> Matrix
addMatrix (matrix1, rows1, cols1) (matrix2, rows2, cols2)
    | (rows1 > 0) && (rows1 == rows2) && (cols1 == cols2) = 
        (zipWith (zipWith (+)) matrix1 matrix2, rows1, cols1)
    | otherwise = error "Matrix addition error"

mulMatrix :: Matrix -> Matrix -> Matrix
mulMatrix (matrix1, rows1, cols1) (matrix2, rows2, cols2)
    | (rows1 > 0) && (rows2 > 0) && 
        (cols1 > 0) && (cols2 > 0) &&
        (cols1 == rows2)  = 
            ([[ sum $ zipWith (*) row1 row2 
                | row2 <- (transpose matrix2)] 
                | row1 <- matrix1 ], 
            rows1, cols2)
    | otherwise = error "Matrix multiplication error"
