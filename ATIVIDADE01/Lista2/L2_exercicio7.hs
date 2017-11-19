ex7 :: Int -> Int -> Int
ex7 n k = product [n,n-1..n-k+1] `div` product [1..k]
  
main = do
  print(ex7 9 4)