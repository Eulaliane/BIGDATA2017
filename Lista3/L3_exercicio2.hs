menorDivisivel :: Integer -> Integer -> Integer
menorDivisivel x y
  | x == y = x
  | otherwise = lcm x (menorDivisivel (x+1) y)

euler5 :: Integer
euler5 = menorDivisivel 1 20

main = do
  print(euler5)