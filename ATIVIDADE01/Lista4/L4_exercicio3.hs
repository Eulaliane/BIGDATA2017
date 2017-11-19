diagonalSec :: [[Integer]] -> Integer
diagonalSec xs = foldl (+) 0 [xs !! x !! (length xs - 1 - x) | x <- [0..(length xs - 1)]]

main = do
  print(diagonalSec [[1,2,3],
                     [4,5,6],
                     [3,2,1]])