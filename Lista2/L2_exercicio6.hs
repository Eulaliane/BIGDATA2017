ex6 :: Integer -> Int
ex6 n = length (somarLista  n) -1

somarLista :: Integer -> [Integer]
somarLista n | n < 10    = [n]
             | otherwise = n: somarLista (somarNumeros n)

somarNumeros :: Integer -> Integer
somarNumeros = sum . numeros

numeros:: Integer -> [Integer]
numeros n = [read [x] | x <- show n]
  
main = do
  print(ex6 198)