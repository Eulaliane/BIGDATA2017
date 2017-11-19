mult5 :: Int -> Bool
mult5 x 
  | x `mod` 5 == 0 = True
  | otherwise = False

main = do
  print(mult5 15)