divisivel20 :: Int -> Bool
n = 20
divisivel20 x 
  | (length [n | n<-[1..n], x `mod` n == 0]) == 20 = True
  | otherwise = False
  
main = do
  print(divisivel20 232792560)