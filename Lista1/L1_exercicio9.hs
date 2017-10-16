ex9 :: [Int]

lista = [ano | ano <- [1..2017], (ano `rem` 400 == 0) || ((ano `rem` 4 == 0) && (ano `rem` 100 /= 0))]

ex9 = drop (length lista - 10) lista


main = do
  print(ex9)