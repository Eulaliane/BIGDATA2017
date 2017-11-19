type Matriz = [[Int]]

matrizIdentidade :: Int -> Matriz

matrizIdentidade n = [ [fromEnum $ i == j | i <- [1..n]] | j <- [1..n]]

main = do
  print(matrizIdentidade 3)