ex8 1 = [1] 
ex8 n = [1] ++ [x+y | (x,y) <- pares (ex8 (n-1))] ++ [1] 
    where pares xs = zip xs (tail xs) 
  
main = do
  print(ex8 8)