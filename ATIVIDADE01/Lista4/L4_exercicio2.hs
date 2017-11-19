diagonalPrincipal :: [[Int]] -> [Int]
diagonalPrincipal xs = zipWith (!!) xs [0..]

somaDiagonalPrincipal :: [[Int]] -> Int
somaDiagonalPrincipal xs = sum(diagonalPrincipal xs)

main = do
  print(somaDiagonalPrincipal [[1,2,3],[4,5,6],[3,2,1]])