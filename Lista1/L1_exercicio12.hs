ex12 :: [Integer]
ex12 = map (\x -> read [x]::Integer) "0123456789"

main = do
  print(ex12)