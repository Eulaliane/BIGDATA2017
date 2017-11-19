ex3 :: [Int]
ex3 = 0 : 1 : next ex3
  where
    next (a : t@(b:_)) = (a+b) : next t

main = do
  print(take 10 ex3)
