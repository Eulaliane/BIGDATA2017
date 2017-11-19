mult35 :: Int -> Bool
mult35 x 
  | (x `mod` 3 == 0) && (x `mod` 5 == 0) = True
  | otherwise = False

main = do
  print(mult35 15)