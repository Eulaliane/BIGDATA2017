type Vetor = [Int]
type Matriz = [[Int]]

numFilas :: Matriz -> Int
numFilas = fst . snd . bounds

numColumnas:: Matriz -> Int
numColumnas = snd . snd . bounds

diagonalSec :: Matriz -> Vector
diagonalSec p = array (1,n) [(i,p!(i,n+1-i)) | i <- [1..n]]
    where n = min (numFilas p) (numColumnas p)

main = do
  print(diagonalSec [[1,2,3],
                     [4,5,6],
                     [3,2,1]])
