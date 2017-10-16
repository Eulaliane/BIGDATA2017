diagonalSecundaria :: [[Int]] -> [[Int]]
diagonalSecundaria [x] = _
diagonalSecundaria (x:xs) = juntar (tail x) (diagonalSecundaria xs)

