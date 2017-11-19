mult3 :: Int -> Bool
mult3 x 
  | x `mod` 3 == 0 = True
  | otherwise = False

main = do
  print(mult3 15)