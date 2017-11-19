ex3 :: [Integer]
ex3 = 0 : 1 : next ex3
  where
    next (a : t@(b:_)) = (a+b) : next t

euler2 :: Integer
euler2 = sum [ x | x <- takeWhile (< 4000000) ex3, even x]

main = do 
  print(euler2)