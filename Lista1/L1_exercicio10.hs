ex10 :: ([Int],[Int])
xs = [ano | ano <- [1..2017], (ano `rem` 400 == 0) || ((ano `rem` 4 == 0) && (ano `rem` 100 /= 0))]
n = (length xs) `div` 2
ex10 = (take n xs, drop n xs)

main = do
  print(ex10)