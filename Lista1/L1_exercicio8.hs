bissexto :: Int -> [Int]

bissexto n = [ano | ano <- [1..n], (ano `rem` 400 == 0) || ((ano `rem` 4 == 0) && (ano `rem` 100 /= 0))]

main = do
  print(bissexto 2017)