ex5 :: [Int] -> [Int] -> Int
ex5 [] [] = 0
ex5 xs ys = sum [x*y | (x,y) <- zip xs ys] 

main = do
  print(ex5 [1,2,3] [4,5,6])