fatores :: Integer -> [Integer]
fatores n = [i | i<-[1..n], n `mod` i == 0]

primo :: Integer -> String
primo n 
  | fatores n == [1, n] = "Numero Primo"
  | otherwise = "Nao eh Numero Primo"
  
main = do
  print(primo 5)