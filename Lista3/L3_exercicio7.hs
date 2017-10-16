collatzLen :: Integer -> Integer
collatzLen x
  | x < 1 = error "Numero menor do que 1"
  | x == 1 = 1
  | x `mod` 2 == 0 = 1 + collatzLen (x `div` 2)
  | otherwise = 1 + collatzLen (3*x + 1)

main = do
  print(collatzLen 15)