ex7 :: Double -> (Double,Double)
ex7 x = (-(sqrt ((1 - cos x)/2)), (sqrt ((1 - cos x)/2)))

main = do
  print(ex7 12)