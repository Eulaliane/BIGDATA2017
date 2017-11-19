ex5 :: Integer -> Integer
ex5 0 = 0
ex5 n = ( n `mod` 10 ) + ex5 ( n `div` 10 ) 
  
main = do
  print(ex5 58)