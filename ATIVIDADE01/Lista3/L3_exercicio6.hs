collatz :: Int -> Int
collatz x 
  | x < 1 = error "Numero menor do que 1"
  | x == 1 = 1
  | x `mod` 2 == 0 = x `div`2
  | otherwise = (3*x+1)`div`2

main = do
  print(collatz 15)