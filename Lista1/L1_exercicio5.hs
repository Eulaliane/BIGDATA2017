ex5 :: Int -> Bool
ex5 x 
  | (x < -1) || ((x > 1) && (x `mod` 2 == 0)) = True
  | otherwise = False

main = do
  print(ex5 12)